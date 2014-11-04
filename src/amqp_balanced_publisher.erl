% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(amqp_balanced_publisher).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/0, add_broker/1, remove_broker/1, publish_headers/5, publish_headers/6]).

-record(abp_state, {
	brokers=[]
}).

%% START API%%
add_broker(Params) ->
	gen_server:call(?MODULE, {add_broker, Params}).

remove_broker(Key) ->
	gen_server:call(?MODULE, {remove_broker, Key}).

publish_headers(sync, MessageId, Headers, Payload, TimeoutMillis) ->
	gen_server:call(?MODULE, {publish_headers, sync, MessageId, Headers, Payload, TimeoutMillis}).

publish_headers(async, MessageId, Headers, Payload, TimeoutMillis, ReplyTag) ->
	gen_server:call(?MODULE, {publish_headers, async, MessageId, Headers, Payload, TimeoutMillis, ReplyTag}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%% END API%%

init([]) ->
	{ok, #abp_state{}}.

pick_broker([]) ->
	no_brokers;

pick_broker([Broker]) ->
	{Broker, []};

pick_broker(Brokers) ->
	%% Just pick one randomly
	Tmp = random:uniform(length(Brokers)),
	Broker = lists:nth(Tmp, Brokers),
	{Broker, lists:delete(Broker, Brokers)}.


loop_attempt_publish_headers(ReturnType, Brokers, Headers, Payload, MessageId, TimeoutMillis, ReplyTag) ->
	case pick_broker(Brokers) of
		no_brokers ->
			{error, no_brokers};
		{{Key, Broker}, RemainingBrokers} ->
			case amqp_managed_connection:publish_headers(Broker, Headers, Payload, MessageId, TimeoutMillis, ReplyTag) of
				not_connected ->
					error_logger:info_msg("amqp_balanced_publisher: Broker ~p returned not_connected while attempting to publish message ~p.  Will retry.~n",[Key, MessageId]),
					loop_attempt_publish_headers(ReturnType, RemainingBrokers, Headers, Payload, MessageId, TimeoutMillis, ReplyTag);
				_Val -> _Val
			end
	end.

handle_call({add_broker, [_AmqpParams, _Exchange, Key] = Params}, _From, #abp_state{brokers=Brokers} = State) ->
	{ok, BrokerPid} = gen_server:start(amqp_managed_connection,Params,[]),
	amqp_managed_connection:register_handler(BrokerPid, disconnected),
	amqp_managed_connection:register_handler(BrokerPid, publish_return),
	amqp_managed_connection:connect(BrokerPid),

	NewBrokers = [{Key, BrokerPid}|Brokers],
	{reply, ok, State#abp_state{brokers=NewBrokers}};

handle_call({remove_broker, Key}, _From, #abp_state{brokers=Brokers} = State) ->  
	Broker = proplists:get_value(Key, Brokers, undefined),
	NewState = case Broker of
		undefined ->
			State;
		_FOUND ->
			amqp_managed_connection:close(Broker),
			State#abp_state{brokers=proplists:delete(Key, Brokers)}
	end,
	{reply, ok, NewState};

handle_call({publish_headers, sync, MessageId, Headers, Payload, TimeoutMillis}, From, State) ->
	case loop_attempt_publish_headers(sync, State#abp_state.brokers, Headers, Payload, MessageId, TimeoutMillis, {sync, From}) of
		ok ->
			{noreply, State};
		publish_wait_timeout ->
			{reply, publish_wait_timeout, State};
		ERROR ->
			{reply, ERROR, State}
	end;
			
handle_call({publish_headers, async, MessageId, Headers, Payload, TimeoutMillis, ReplyTag}, {Pid, _Tag} = _From, State) ->
	case loop_attempt_publish_headers(async, State#abp_state.brokers, Headers, Payload, MessageId, TimeoutMillis, {async, Pid, ReplyTag}) of
		ok ->
			{reply, ok, State};
		publish_wait_timeout ->
			{reply, publish_wait_timeout, State};
		ERROR ->
			{reply, ERROR, State}
	end;

handle_call(Msg, _From, State) ->
	error_logger:info_msg("amqp_balanced_publisher: Unknown handle_call: ~p~n",[Msg]),
	{reply, {error, unknown}, State}.

%% Handle the publish_confirm response and pass it back up the chain to pe_msg_intake %%
handle_cast({publish_confirm, MessageId, ReplyTag}, State) ->
	do_notify(MessageId, publish_confirm, ReplyTag),
	{noreply, State};

%% Handle the publish_reject response and pass it back up the chain to pe_msg_intake %%
handle_cast({publish_reject, MessageId, ReplyTag}, State) ->
	do_notify(MessageId, publish_reject, ReplyTag),
	{noreply, State};

handle_cast(Msg, State) ->
	error_logger:info_msg("amqp_balanced_publisher: Unknown handle_cast: ~p~n",[Msg]),
	{noreply, State}.

do_notify(MessageId, Response, ReplyTo) ->
	case ReplyTo of
		{sync, From} ->
			gen_server:reply(From, {Response, MessageId});
		{async, Pid, ReplyTag} ->
			gen_server:cast(Pid, {Response, MessageId, ReplyTag})
	end,
	ok.

handle_info({disconnected,{key, Key},{pid, BrokerPid}}, #abp_state{brokers=Brokers} = State) ->
	error_logger:info_msg("amqp_balanced_publisher: Received disconnected message from broker for key ~p as pid ~p~n",[Key, BrokerPid]),
	%% Only reconnect if the broker is found in the list of brokers.  It will be missing if the broker was removed and that is why it stopped...
	case proplists:get_value(Key, Brokers, undefined) of
		undefined ->
			ok;
		_FOUND ->
			amqp_managed_connection:connect(BrokerPid)
	end,
	{noreply, State};

handle_info({publish_return,_Key,_BrokerPid,{message_id, MessageIdBin}}, State) ->
	MessageId = binary_to_list(MessageIdBin),
	pe_audit:log([{msg,MessageId}],"MESSAGE-DISCARDED-NO-SUBSCRIBERS"),
	{noreply, State};

handle_info({publish_wait_timeout, MessageId, ReplyTag}, State) ->
	do_notify(MessageId, publish_timeout, ReplyTag),
	{noreply, State};

handle_info({publish_confirm, MessageId, ReplyTag}, State) ->
	do_notify(MessageId, publish_confirm, ReplyTag),
	{noreply, State};

handle_info({publish_reject, MessageId, ReplyTag}, State) ->
	do_notify(MessageId, publish_reject, ReplyTag),
	{noreply, State};

handle_info(Msg, State) ->
	error_logger:info_msg("amqp_balanced_publisher: Unknown handle_info: ~p~n",[Msg]),
	{noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.




