% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_sub_store).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
 
-export([
	cancel/1, 
	create_new/1, 
	find_by_duplication_key/1,
	find_by_message_type/1,
	get_all_ids/0, 
	get_all_for_principal/1, 
	get_all_active_for_principal/1, 
	get_all_subscriptions/0,
	lookup/1, 
	lookup_string/1, 
	start_link/0
]).

-export([no_gen_server_lookup_all/0, no_gen_server_lookup/1]).

-include("include/prospero.hrl").
 
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_new(Subscription) ->
  gen_server:call(?MODULE, {new_sub, Subscription}).

lookup(SubId) ->
  gen_server:call(?MODULE, {lookup, SubId}).
  
lookup_string(SubId) ->
  %Returns a string instead of a record
  gen_server:call(?MODULE, {lookup_string, SubId}).

get_all_ids() ->
  gen_server:call(?MODULE, {lookup_all}).
  
cancel(Subscription) ->
  gen_server:call(?MODULE, {cancel, Subscription}).
  
get_all_for_principal(PrincipalId) ->
  gen_server:call(?MODULE, {get_all_for_principal, PrincipalId}).
  
get_all_active_for_principal(PrincipalId) ->
  gen_server:call(?MODULE, {get_all_active_for_principal, PrincipalId}).
  
find_by_duplication_key(Key) ->
  gen_server:call(?MODULE, {find_by_duplication_key, Key}).

no_gen_server_lookup_all() ->
  mnesia:dirty_all_keys(pe_sub).

find_by_message_type(MessageType) ->
	gen_server:call(?MODULE, {find_by_message_type, MessageType}).

no_gen_server_lookup(SubId) ->
  case mnesia:dirty_read(pe_sub, SubId) of
    [FoundSub] ->
      {found, FoundSub};
    [] ->
      none
  end.
  
init([]) ->
  mnesia:subscribe({table, pe_sub, simple}),
  {ok, []}.
  
  
  
 
handle_call({new_sub, Subscription}, _From, State) ->
  Fun = fun() ->
	  Id = case Subscription#pe_sub.id == undefined of
		  true -> 
			  {id,TempId} = pe_id_broker:get_next(),
			  TempId;
		  _ -> Subscription#pe_sub.id
	  end,
    QueueName = "Sub-" ++ Id,
	  UpdatedSub = case Subscription#pe_sub.date_created == undefined of
		  true -> Subscription#pe_sub{id=Id,queue_name=QueueName, date_created=pe_time:current_timestamp()} ;
		  _ -> Subscription#pe_sub{id=Id, queue_name=QueueName}
	  end,
    mnesia:write(UpdatedSub),
    {created, UpdatedSub}
  end,
  
  Reply = case mnesia:transaction(Fun) of
    {atomic, {created, UpdatedSub}} ->
      {created, UpdatedSub}
  end,
   
  {reply, Reply, State};

handle_call({get_all_for_principal, PrincipalId}, _From, _State) ->
  Result = mnesia:dirty_match_object(pe_sub, {pe_sub, '_', PrincipalId, '_', '_', '_', '_', '_', '_', '_'}),
  {reply, Result, _State};
  
handle_call({get_all_active_for_principal, PrincipalId}, _From, _State) ->
  Result = mnesia:dirty_match_object(pe_sub, {pe_sub, '_', PrincipalId, '_', '_', '_', '_', '_', undefined, '_'}),
  {reply, Result, _State};

handle_call({find_by_duplication_key, Key}, _From, _State) ->
  Result = mnesia:dirty_match_object(pe_sub, {pe_sub, '_', '_', '_', '_', '_', '_', '_', '_', Key}),
  {reply, Result, _State};

handle_call({find_by_message_type, MessageType}, _From, State) ->
	Result = do_find_by_message_type(MessageType),
	{reply, Result, State};

handle_call({lookup, Id}, _From, State) ->
  Reply = no_gen_server_lookup(Id),
  {reply, Reply, State};

handle_call({lookup_string, Id}, _From, State) ->
  case no_gen_server_lookup(Id) of
    {found, Sub} ->
      Start = io_lib:format("Subscription ~s:",[Id]),
      {_Counter, Result} = lists:foldl(fun(Field, {Counter,Accum}) -> {Counter + 1, io_lib:format("~s~n    ~s: ~p",[Accum, Field, element(Counter, Sub)])} end, {2,Start}, record_info(fields, pe_sub)),
      {reply, list_to_binary(Result), State};
    none ->
      {reply, list_to_binary(io_lib:format("Error looking up Subscription ~s: not found",[Id])), State}
  end;

handle_call({lookup_all}, _From, _State) ->
  {reply, no_gen_server_lookup_all(), _State};
  
handle_call({cancel, Subscription}, _From, State) ->
  Fun = fun() ->
    UpdatedSub = Subscription#pe_sub{date_cancelled=pe_time:current_timestamp()},
    %mnesia:write(UpdatedSub), %A logic delete, but keep the record.
    Id = pe_sub:get(id, Subscription),
    mnesia:delete({pe_sub, Id}),
    {cancelled, UpdatedSub}
  end,        
  Reply = mnesia:sync_dirty(Fun),
  {reply, Reply, State}.
 
handle_cast(_Msg, State) -> {noreply, State}.

handle_info({mnesia_table_event, {delete, {pe_sub, SubId}, _ActivityId}}, State) ->
  event_manager:notify({sub_cancelled, SubId}),
  {noreply, State};

handle_info({mnesia_table_event, {write, Subscription, _ActivityId}}, State) ->
  event_manager:notify({sub_created, Subscription}),
  {noreply, State};

handle_info(_Msg, State) ->
  error_logger:info_msg("pe_sub_store:  Unknown handle_info: ~p~n", [_Msg]),
  {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

do_find_by_message_type (MessageType) -> 
	AllSubscriptions = mnesia:dirty_all_keys(pe_sub),
	find_subscriptions_for_message_type(AllSubscriptions, MessageType).


find_subscriptions_for_message_type([], _MessageType) -> [];
find_subscriptions_for_message_type([Key | Rest], MessageType) -> 
	Result = check_subscription(Key, MessageType),
	case Result of
		no_match -> find_subscriptions_for_message_type(Rest, MessageType);
		_ -> [ Result | find_subscriptions_for_message_type(Rest, MessageType) ]
	end.

check_subscription(Key, MessageType) ->
	[ Subscription ] = mnesia:dirty_read(pe_sub, Key),
	case subscription_matches_message_type(Subscription, MessageType) of
		true -> Subscription;
		_ -> no_match
	end.

subscription_matches_message_type(Subscription, MessageType) ->
	Target = "MessageType:" ++ MessageType,
	Matches = lists:filter (
		fun(T) -> T == Target end,
		Subscription#pe_sub.tag_ids
	),
	case Matches of
		[] -> false;
		_ -> true
	end.

%
% return a list of all the subscriptions in the database
%
get_all_subscriptions() ->
	AllIds = mnesia:dirty_all_keys(pe_sub),
	lists:map(
		fun (Key) ->
			[ Subscription ] = mnesia:dirty_read(pe_sub, Key),
			Subscription
		end,
		AllIds
	).

