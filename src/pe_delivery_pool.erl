% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_delivery_pool).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
	deliver/4,
	start_link/0,
	list_in_flight_deliveries/0
]).

-define(DEFAULT_NUM_DELIVERERS, 1000).

-record(pedp_state, {
	deliverers=undefined
}).

%% START API%%
deliver(Subscription, Message, Secret, From) ->
	gen_server:cast(?MODULE, {deliver, Subscription, Message, Secret, From}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

list_in_flight_deliveries() ->
	gen_server:call(?MODULE, list_if_deliveries).
%% END API%%

init([]) ->
	NumWorkers = pe_config:get(delivery, number_of_workers, ?DEFAULT_NUM_DELIVERERS),
	{ok, #pedp_state{
		deliverers=init_deliverers(undefined, NumWorkers)  
	}}.

init_deliverers(undefined, TotalToStart) ->
	init_deliverers(queue:new(), TotalToStart);

init_deliverers(Running, TotalToStart) ->
	case queue:len(Running) == TotalToStart of
		true ->
			Running;
		false ->
			{ok, DelivererPid} = gen_server:start(pe_delivery,[],[]),
			init_deliverers(queue:in(DelivererPid, Running), TotalToStart)
	end.

%% This is only used for debugging...
handle_call(list_if_deliveries, _From, State) ->
	TmpAsList = queue:to_list(State#pedp_state.deliverers),
	Result = lists:foldl(fun(E, Accum) -> [{E, pe_delivery:list_in_flight_deliveries(E)}|Accum] end, [], TmpAsList),
	{reply, {ok, Result}, State};

handle_call(Msg, _From, State) ->
	error_logger:info_msg("pe_delivery_pool: Unknown handle_call from ~p: ~p~n",[_From, Msg]),
	{reply, unknown, State}.

handle_cast({deliver, Subscription, MessageBin, Secret, From}, #pedp_state{deliverers=Deliverers} = State) ->
	{{value, Deliverer}, NewQueue} = queue:out(Deliverers),
	pe_delivery:deliver(Deliverer, Subscription, MessageBin, Secret, From),
	{noreply, State#pedp_state{deliverers=queue:in(Deliverer, NewQueue)}};

handle_cast(Msg, State) ->
	error_logger:info_msg("pe_delivery_pool: Unknown handle_cast: ~p~n",[Msg]),
	{noreply, State}.

handle_info(Msg, State) ->
	error_logger:info_msg("pe_delivery_pool: Unknown handle_info: ~p~n",[Msg]),
	{noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

	
