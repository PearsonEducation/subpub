% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_principal_store).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
 
-export([
	activate/1, 
	create_new/1, 
	deactivate/1, 
	delete/1,
	get_all_ids/0,
	get_all_principals/0,
	lookup/1, 
	lookup_string/1, 
	start_link/0
]).

-export([no_gen_server_lookup_all/0, no_gen_server_lookup/1]).

-include("include/prospero.hrl").
 
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_new(Principal) ->
  gen_server:call(?MODULE, {new, Principal}).

lookup(PrincipalId) ->
  gen_server:call(?MODULE, {lookup, PrincipalId}).

lookup_string(PrincipalId) ->
  %Returns a string instead of a record
  gen_server:call(?MODULE, {lookup_string, PrincipalId}).
  
get_all_ids() ->
  gen_server:call(?MODULE, {lookup_all}).

get_all_principals() ->
	gen_server:call(?MODULE, {get_all}).
  
deactivate(Principal) ->
  gen_server:call(?MODULE, {deactivate, Principal}).

activate(Principal) ->
  gen_server:call(?MODULE, {activate, Principal}).

delete(PrincipalId) ->
	gen_server:call(?MODULE, {delete, PrincipalId}).


init([]) ->
  {ok, []}.
  

handle_call({new, Principal}, _From, State) ->
  Fun = fun() ->
    Updated = case Principal#pe_principal.date_created == undefined of 
		true -> Principal#pe_principal{date_created=pe_time:current_timestamp()} ;
		_ -> Principal
	end,
    mnesia:write(Updated),
    {created, Updated}
  end,        
  Reply = mnesia:sync_dirty(Fun),
  {reply, Reply, State};

  
handle_call({lookup, Id}, _From, State) ->
  Reply = no_gen_server_lookup(Id),
  {reply, Reply, State};

handle_call({lookup_string, Id}, _From, State) ->
  case no_gen_server_lookup(Id) of
    {found, P} ->
      Start = io_lib:format("Principal ~s:",[Id]),
      {_Counter, Result} = lists:foldl(fun(Field, {Counter,Accum}) -> {Counter + 1, io_lib:format("~s~n    ~s: ~p",[Accum, Field, element(Counter, P)])} end, {2,Start}, record_info(fields, pe_principal)),
      {reply, list_to_binary(Result), State};
    none ->
      {reply, list_to_binary(io_lib:format("Error looking up Principal ~s: not found",[Id])), State}
  end;


handle_call({lookup_all}, _From, _State) ->
  {reply, no_gen_server_lookup_all(), _State};

handle_call({get_all}, _From, _State) ->
	{ reply, get_all(), _State };

handle_call({deactivate, Principal}, _From, State) ->
  Fun = fun() ->
    Updated = Principal#pe_principal{date_deactivated=pe_time:current_timestamp()},
    mnesia:write(Updated),
    {cancelled, Updated}
  end,        
  Reply = mnesia:sync_dirty(Fun),
  {reply, Reply, State};

handle_call({activate, Principal}, _From, State) ->
  Fun = fun() ->
    Updated = Principal#pe_principal{date_deactivated=undefined},
    mnesia:write(Updated),
    {cancelled, Updated}
  end,        
  Reply = mnesia:sync_dirty(Fun),
  {reply, Reply, State};

handle_call({delete, PrincipalId}, _From, State) ->
	Fun = fun() ->
		mnesia:delete({pe_principal, PrincipalId}),
		{ deleted, PrincipalId }
	end,
	Reply = mnesia:sync_dirty(Fun),
	{ reply, Reply, State }. 

 
no_gen_server_lookup_all() ->
  mnesia:dirty_all_keys(pe_principal).
  
no_gen_server_lookup(Id) ->
  case mnesia:dirty_read(pe_principal, Id) of
    [Found] ->
      {found, Found};
    [] ->
      none
  end.

%
% return a list containing all the principals in the database
%
get_all() ->
	AllIds = mnesia:dirty_all_keys(pe_principal),
	lists:map(
		fun (Key) ->
			[ Subscription ] = mnesia:dirty_read(pe_principal, Key),
			Subscription
		end,
		AllIds
	).

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
