% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(amqp_manager).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export[
	make_broker_params/2,
	start_link/0,
	register_broker/2, 
	publish_to_broker/1, 
	stop_publishing_to_broker/1,
	consume_from_broker/1,
	watch_subscription/1,
	stop_consumers_at_broker/1,
	unwatch_subscription/2,
	unwatch_all/2,
	watch_all/0,
	broker_status/0
].

-record(am_state, {
brokers=[],
publishing=[],
consuming=[]
}).

-include_lib("amqp_client/include/amqp_client.hrl").

%% START API%%
make_broker_params(Key, Config) ->
  [#amqp_params_network{
    host=proplists:get_value(host, Config, undefined),
    username=list_to_binary(proplists:get_value(username, Config, "guest")),
    password=list_to_binary(proplists:get_value(password, Config, "guest")),
    port=proplists:get_value(port, Config, undefined),
    virtual_host=list_to_binary(proplists:get_value(virtual_host, Config, "/")),
    heartbeat=proplists:get_value(heartbeat_milliseconds, Config, undefined)
  }, proplists:get_value(exchange, Config, "undefined"), Key].

register_broker(Key, Params) ->
	gen_server:call(?MODULE, {register, Key, Params}).

publish_to_broker(Key) ->
	gen_server:call(?MODULE, {publish_to, Key}).

stop_publishing_to_broker(Key) ->
	gen_server:call(?MODULE, {stop_publish_to, Key}).

consume_from_broker(Key) ->
	gen_server:call(?MODULE, {consume_from, Key}).

stop_consumers_at_broker(Key) ->
	gen_server:call(?MODULE, {stop_consuming_from, Key}).

watch_subscription({id, SubId}) ->
	case pe_sub_store:lookup(SubId) of
		{found, Sub} ->
			watch_subscription(Sub);
		none ->
			{error, sub_not_found}
	end;

watch_subscription(Subscription) ->
	gen_server:call(?MODULE, {watch, Subscription}).

unwatch_subscription(SubId, DeleteQueue) ->
	gen_server:call(?MODULE, {unwatch, SubId, DeleteQueue}).

unwatch_all(DeleteQueue, Timeout) ->
	gen_server:call(?MODULE, {unwatch, all, DeleteQueue, Timeout}, Timeout).

watch_all() ->
  lists:foreach(fun(SubId) ->
    case pe_sub_store:lookup(SubId) of
      {found, Sub} ->
        amqp_manager:watch_subscription(Sub);
      _ELSE ->
        erlang:error({sub_id_found_but_record_missing, SubId})
    end
  end, pe_sub_store:get_all_ids()),
  ok.

broker_status() ->
	gen_server:call(?MODULE, broker_status).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%% END API%%

init([]) ->
	{ok, #am_state{}}.

handle_call(broker_status, _From, State) ->
	Result = [
		{brokers, State#am_state.brokers},
		{publishing_to, proplists:get_keys(State#am_state.publishing)},
		{consuming_from, proplists:get_keys(State#am_state.consuming)}
	],
	{reply, {ok, Result}, State};

handle_call({publish_to, Key}, _From, #am_state{brokers=Brokers, publishing=Publishing} = State) ->
	case proplists:is_defined(Key, Publishing) of
		true ->
			{reply, ok, State};
		false ->
			case proplists:get_value(Key, Brokers, undefined) of
				undefined ->
					{reply, {error, unknown_broker}, State};
				Broker ->
					amqp_balanced_publisher:add_broker(Broker),
					{reply, ok, State#am_state{publishing=[{Key, ok}|Publishing]}}
			end
	end;

handle_call({consume_from, Key}, _From, #am_state{brokers=Brokers, consuming=Consuming} = State) ->
	case proplists:is_defined(Key, Consuming) of
		true ->
			{reply, ok, State};
		false ->
			case proplists:get_value(Key, Brokers, undefined) of
				undefined ->
					{reply, {error, unknown_broker}, State};
				Broker ->
					amqp_consume_manager:add_broker(Broker),
					{reply, ok, State#am_state{consuming=[{Key, ok}|Consuming]}}
			end
	end;

handle_call({stop_consuming_from, Key}, _From, #am_state{consuming=Consuming} = State) ->
	amqp_consume_manager:remove_broker(Key),
	{reply, ok, State#am_state{consuming=proplists:delete(Key, Consuming)}};

handle_call({watch, SubId}, _From, State) when is_list(SubId) ->
	case pe_sub_store:lookup(SubId) of
		{found, Sub} ->
			handle_call({watch, Sub}, _From, State);
		_ELSE ->
			{reply, {error, not_found}, State}
	end;

handle_call({watch, Subscription}, _From, State) ->
	amqp_consume_manager:watch_subscription(Subscription, lists:map(fun({Key, ok}) -> proplists:get_value(Key, State#am_state.brokers, undefined) end, State#am_state.consuming)),
	{reply, ok, State};

handle_call({unwatch, all, DeleteQueue, Timeout}, _From, State) ->
	amqp_consume_manager:stop_watching_all(DeleteQueue, Timeout),
	{reply, ok, State};

handle_call({unwatch, SubId, DeleteQueue}, _From, State) ->
	amqp_consume_manager:stop_watching_subscription(SubId, DeleteQueue),
	{reply, ok, State};

handle_call({stop_publish_to, Key}, _From, #am_state{publishing=Publishing} = State) ->
	amqp_balanced_publisher:remove_broker(Key),
	{reply, ok, State#am_state{publishing=proplists:delete(Key, Publishing)}};
			
handle_call({register, Key, Params}, _From, #am_state{brokers=Brokers} = State) ->
	{reply, ok, State#am_state{brokers=[{Key, Params}|Brokers]}};

handle_call(Msg, _From, State) ->
	error_logger:info_msg("amqp_manager: Unknown handle_call: ~p~n",[Msg]),
	{reply, {error, unknown}, State}.

handle_cast(Msg, State) ->
	error_logger:info_msg("amqp_manager: Unknown handle_cast: ~p~n",[Msg]),
	{noreply, State}.

handle_info(Msg, State) ->
	error_logger:info_msg("amqp_manager: Unknown handle_info: ~p~n",[Msg]),
	{noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.




