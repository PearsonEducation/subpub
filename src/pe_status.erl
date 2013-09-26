% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_status).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
 
-export([start_link/0, get_local_status/0, get_aggregated_cluster_status/0, record/1]).

-include("include/prospero.hrl").

get_local_status() -> gen_server:call(?MODULE, get_local_status).

get_aggregated_cluster_status() ->
  {Replies,BadNodes} = gen_server:multi_call([node() | nodes()], ?MODULE, get_local_status, 1000),
  {Replies,BadNodes}.
  
record(Item) -> gen_server:call(?MODULE, {record, Item}).
  
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, #status{node=node(), version=?PROSPERO_VERSION, num_messages_posted=0, num_messages_delivered=0, num_failed_deliveries=0, time_started=pe_time:current_timestamp(), uptime=0, num_watched_subscriptions=0, subscription_pids=[]}}.
 
handle_call({record, msg_posted}, _From, State) ->
  Tmp = State#status.num_messages_posted + 1,
  {reply, ok, State#status{num_messages_posted=Tmp}};

handle_call({record, msg_delivered}, _From, State) ->
  Tmp = State#status.num_messages_delivered + 1,
  {reply, ok, State#status{num_messages_delivered=Tmp}};

handle_call({record, delivery_failed}, _From, State) ->
  Tmp = State#status.num_failed_deliveries + 1,
  {reply, ok, State#status{num_failed_deliveries=Tmp}};

handle_call({record, {broker_connection_failed, Pid, Host, Description, WillTryAgainAt, NumFailedAttempts}}, _From, State) ->
  Tmp = State#status.pids_awaiting_broker_reconnection,
  Tmp2 = proplists:delete(Pid, Tmp),
  Tmp3 = [{Pid, {Host, Description, WillTryAgainAt, NumFailedAttempts}} | Tmp2],
  {reply, ok, State#status{pids_awaiting_broker_reconnection=Tmp3}};

handle_call({record, {broker_connection, Pid}}, _From, State) ->
  Tmp = State#status.pids_awaiting_broker_reconnection,
  Tmp2 = proplists:delete(Pid, Tmp),
  {reply, ok, State#status{pids_awaiting_broker_reconnection=Tmp2}};
  
handle_call(get_local_status, _From, State) ->
  Uptime = pe_time:current_timestamp() - State#status.time_started,
  {ok, SubPids} = amqp_consume_manager:get_watched_subscription_status(),
  NumWatchedSubs = length(SubPids),
  RestStatus = pe_config:get(rest,status,undefined),
  UpdatedState = State#status{uptime=Uptime, num_watched_subscriptions=NumWatchedSubs, subscription_pids=SubPids, rest_status=RestStatus},
  
  {reply, {ok, UpdatedState}, UpdatedState}.
 
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
