% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(amqp_consume_manager).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/0, add_broker/1, remove_broker/1, watch_subscription/2, stop_watching_subscription/2, ack/1, reject/2, get_watched_subscription_status/0, stop_watching_all/2]).

-record(acm_state, {
conns_by_broker_pid=[],
watched_subs=[],
brokers=[]
}).

%% START API%%
add_broker(Params) ->
	gen_server:call(?MODULE, {add_broker, Params}).

remove_broker(Key) ->
	gen_server:call(?MODULE, {remove_broker, Key}).

watch_subscription(Subscription, Brokers) ->
	gen_server:call(?MODULE, {watch_sub, Subscription, Brokers}).

stop_watching_subscription(SubscriptionId, DeleteQueue) ->
	gen_server:call(?MODULE, {stop_sub, SubscriptionId, DeleteQueue}).

stop_watching_all(DeleteQueue, Timeout) ->
	gen_server:call(?MODULE, {stop_sub, all, DeleteQueue}, Timeout).

ack(MessageTag) ->
	gen_server:cast(?MODULE, {ack, MessageTag}).

reject(MessageTag, OriginalPayload) -> 
	gen_server:cast(?MODULE, {reject, MessageTag, OriginalPayload}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_watched_subscription_status() ->
	gen_server:call(?MODULE, watched_subs).
%% END API%%

init([]) ->
	{ok, #acm_state{}}.

is_watching_subscription_at_broker(_Key, _SubId, []) ->
	false;
is_watching_subscription_at_broker(_Key, _SubId, [{_Pid,{_Key,_SubId}}|_Remaining]) ->
	true;
is_watching_subscription_at_broker(Key, SubId, [{_Pid,{_Key,_SubId}}|Remaining]) ->
	is_watching_subscription_at_broker(Key, SubId, Remaining).
	
add_connection([_AmqpParams, _Exchange, Key] = BrokerParams, Subscription, State) ->
	SubId = pe_sub:get(id, Subscription),

	case is_watching_subscription_at_broker(Key, SubId, State#acm_state.conns_by_broker_pid) of
		true ->
			State;
		false ->
			error_logger:info_msg("amqp_consume_manager: Adding connection for sub ~p and broker ~p~n", [SubId, Key]),

			{ok, BrokerPid} = gen_server:start(amqp_managed_connection,BrokerParams,[]),

			amqp_managed_connection:set_dog_tag(BrokerPid, {Key, SubId}),
			amqp_managed_connection:register_handler(BrokerPid, connected),
			amqp_managed_connection:register_handler(BrokerPid, disconnected),
			amqp_managed_connection:register_handler(BrokerPid, message),
			amqp_managed_connection:connect(BrokerPid),

			NewConnsByBrokerPid = [{BrokerPid, {Key, SubId}}|State#acm_state.conns_by_broker_pid],
			NewWatchedSubs = [{SubId, Subscription}|proplists:delete(SubId, State#acm_state.watched_subs)],
			NewBrokers = [{Key, BrokerParams}|proplists:delete(Key, State#acm_state.brokers)],

			State#acm_state{
				brokers=NewBrokers,
				conns_by_broker_pid=NewConnsByBrokerPid, 
				watched_subs=NewWatchedSubs
			}
	end.

recursive_add_connection_for_broker(_BrokerParams, [], State) ->
	State;
recursive_add_connection_for_broker(BrokerParams, [{_SubId,Sub}|Subscriptions], State) ->
	NewState = add_connection(BrokerParams, Sub, State),
	recursive_add_connection_for_broker(BrokerParams, Subscriptions, NewState).

recursive_add_connection_for_sub([], _Subscription, State) ->
	State;
recursive_add_connection_for_sub([BrokerParams|Brokers], Subscription, State) ->
	NewState = add_connection(BrokerParams, Subscription, State),
	recursive_add_connection_for_sub(Brokers, Subscription, NewState).

do_stop_all([], _DeleteQueue) ->
	ok;
do_stop_all([BrokerPid|Remaining], DeleteQueue) ->
	error_logger:info_msg("amqp_consume_manager: Requesting close connection consumer of sub at broker pid ~p~n",[BrokerPid]),

	case DeleteQueue of
		true ->
			try
				amqp_managed_connection:delete_queue(BrokerPid)
			catch
				exit:EXIT ->
					self() ! {disconnected,{pid, BrokerPid}},
					error_logger:error_msg("amqp_consume_manager: Exit exception while attempting to delete queue for consumer pid ~p: ~p~n",[BrokerPid, EXIT])
			end;
		false ->
			ok
	end,
	try
		amqp_managed_connection:close(BrokerPid)
	catch
		exit:EXIT2 ->
			self() ! {disconnected,{pid, BrokerPid}},
			error_logger:error_msg("amqp_consume_manager: Exit exception while attempting to call amqp_managed_connection:close(~p): ~p~n",[BrokerPid, EXIT2])
	end,
	do_stop_all(Remaining, DeleteQueue).

handle_call({add_broker, [_AmqpParams, _Exchange, Key] = Params}, _From, State) ->
	error_logger:info_msg("amqp_consume_manager: Adding broker: ~p~n",[Params]),
	NewState = recursive_add_connection_for_broker(Params, State#acm_state.watched_subs, State),
	NewBrokers = [{Key, Params}|proplists:delete(Key, State#acm_state.brokers)],
	{reply, ok, NewState#acm_state{brokers=NewBrokers}};

handle_call({remove_broker, Key}, _From, State) -> 
	error_logger:info_msg("amqp_consume_manager: Removing broker: ~p~n",[Key]),
	
	ConnectionsForBroker = lists:filter(fun({_ItemPid, {ItemKey, _SubId}}) -> ItemKey == Key end, State#acm_state.conns_by_broker_pid),
	AsListOfPids = lists:map(fun({ItemPid, {_ItemKey, _SubId}}) -> ItemPid end, ConnectionsForBroker),
	
	do_stop_all(AsListOfPids, false),
	NewBrokers = proplists:delete(Key, State#acm_state.brokers),
	{reply, ok, State#acm_state{brokers=NewBrokers}};

handle_call({watch_sub, Subscription, Brokers}, _From, State) ->
	NewState = recursive_add_connection_for_sub(Brokers, Subscription, State),
	{reply, ok, NewState};

handle_call({stop_sub, all, DeleteQueue}, _From, State) ->
	error_logger:info_msg("amqp_consume_manager: Stopping all consumers~n",[]),

	AsListOfPids = lists:map(fun({ItemPid, {_ItemKey, _SubId}}) -> ItemPid end, State#acm_state.conns_by_broker_pid),

	do_stop_all(AsListOfPids, DeleteQueue),
	{reply, ok, State#acm_state{watched_subs=[]}};

handle_call({stop_sub, SubId, DeleteQueue}, _From, State) ->
	error_logger:info_msg("amqp_consume_manager: Stopping all consumers for subscription: ~p~n",[SubId]),

	ConnectionsForBroker = lists:filter(fun({_ItemPid, {_ItemKey, ItemSubId}}) -> ItemSubId == SubId end, State#acm_state.conns_by_broker_pid),
	AsListOfPids = lists:map(fun({ItemPid, {_ItemKey, _SubId}}) -> ItemPid end, ConnectionsForBroker),

	do_stop_all(AsListOfPids, DeleteQueue),
	NewSubs = proplists:delete(SubId, State#acm_state.watched_subs),
	{reply, ok, State#acm_state{watched_subs=NewSubs}};
	
handle_call(watched_subs, _From, State) ->
	ConnsBySubId = lists:foldl(
		fun({Pid, {_Key,SubId}}, Accum) ->
			Tmp = [Pid|proplists:get_value(SubId, Accum, [])],
			[{SubId, Tmp}|proplists:delete(SubId, Accum)]
		end, [], State#acm_state.conns_by_broker_pid),
	{reply, {ok, ConnsBySubId}, State};

handle_call(Msg, _From, State) ->
	error_logger:info_msg("amqp_consume_manager: Unknown handle_call: ~p~n",[Msg]),
	{reply, {error, unknown}, State}.

handle_cast({ack, [{broker,Broker}, {delivery_tag, Tag}] = _MessageTag}, State) ->
	amqp_managed_connection:ack(Broker, Tag),
	{noreply, State};

handle_cast({reject, [{broker,Broker}, {delivery_tag, Tag}], OriginalPayload}, State) ->
	amqp_managed_connection:reject(Broker, Tag, pe_util:increment_failed_attempt_count_and_set_last_attempted_timestamp(OriginalPayload)),
	{noreply, State};

handle_cast(Msg, State) ->
	error_logger:info_msg("amqp_consume_manager: Unknown handle_cast: ~p~n",[Msg]),
	{noreply, State}.

handle_info({disconnected,{pid, BrokerPid}}, State) ->
	case proplists:get_value(BrokerPid, State#acm_state.conns_by_broker_pid, undefined) of
		undefined ->
			{noreply, State};
		{Key, _SubId} ->
			handle_info({disconnected,{key, Key},{pid, BrokerPid}}, State)
	end;

handle_info({disconnected,{key, Key},{pid, BrokerPid}}, #acm_state{brokers=Brokers, watched_subs=WatchedSubs} = State) ->
	error_logger:info_msg("amqp_consume_manager: Received disconnected message from broker for key ~p as pid ~p~n",[Key, BrokerPid]),
	%% Only reconnect if the broker is found in the list of brokers or the subscription is found in the list of watched subscriptions.
	%% It will be missing if the broker or sub was removed and that is why it stopped...
	case proplists:get_value(Key, Brokers, undefined) of
		undefined ->
			ok;
		_FOUND1 ->
			case proplists:get_value(BrokerPid, State#acm_state.conns_by_broker_pid, none) of
				none ->
					ok;
				{_Key, SubId} ->
					case proplists:get_value(SubId, WatchedSubs, undefined) of
						undefined ->
							ok;
						_FOUND2 ->
							amqp_managed_connection:connect(BrokerPid)
					end
			end
	end,

	{noreply, State#acm_state{
		conns_by_broker_pid = proplists:delete(BrokerPid, State#acm_state.conns_by_broker_pid)
	}};

handle_info({connected,{key, Key},{pid, BrokerPid}}, #acm_state{watched_subs=WatchedSubs} = State) ->
	error_logger:info_msg("amqp_consume_manager: Connection for broker pid ~p is connected~n",[BrokerPid]),
	{ok, {Key, SubId}} = amqp_managed_connection:get_dog_tag(BrokerPid),

	NewState = case proplists:get_value(SubId, WatchedSubs, undefined) of
		undefined ->
			State;
		Subscription ->
			case pe_sub_store:lookup(pe_sub:get(id, Subscription)) of
				{found, _Sub} ->
					case pe_principal_store:lookup(pe_sub:get(principal_id, Subscription)) of
						{found, Principal} ->
							PrincipalReam = pe_principal:get(realm, Principal),
							PrincipalSecret = pe_principal:get(secret, Principal),

							amqp_managed_connection:declare_queue(BrokerPid, pe_sub:get(queue_name, Subscription), pe_sub:get(tags, Subscription)),
							amqp_managed_connection:consume_queue(BrokerPid, pe_sub:get(queue_name, Subscription), PrincipalReam, pe_sub:get(id, Subscription), PrincipalSecret),

							NewConnsByBrokerPid = [{BrokerPid, {Key, SubId}}|proplists:delete(BrokerPid, State#acm_state.conns_by_broker_pid)],
							State#acm_state{conns_by_broker_pid=NewConnsByBrokerPid};
						none ->
							error_logger:error_msg("amqp_consume_manager:  Cannot consume queue for subscription: Principal not found: ~p~n",[Subscription]),
							amqp_managed_connection:close(BrokerPid),
							State
					end;
				none ->
					error_logger:error_msg("amqp_consume_manager:  Cannot consume queue for missing subscription: Was it recently deleted? Closing connection...~p~n",[Subscription]),
					amqp_managed_connection:close(BrokerPid),
					State
			end					
	end,
	{noreply, NewState};

handle_info({message, Key, {pid, BrokerPid}, [{message_id, MessageId},{delivery_tag,DeliveryTag},{content, Content},{principal_secret, Secret}]}, State) ->
	case proplists:get_value(BrokerPid, State#acm_state.conns_by_broker_pid, none) of
		none ->
			ok;
		{_Key, SubId} ->
			case proplists:get_value(SubId, State#acm_state.watched_subs, undefined) of
				undefined ->
					error_logger:error_msg("amqp_consume_manager: Received message ~p for unknown subscription ~p from broker ~p pid ~p~n", [MessageId, SubId, Key, BrokerPid]),
					try
						amqp_managed_connection:reject(BrokerPid, DeliveryTag)
					catch
						exit:EXIT ->
							self() ! {disconnected,{key, Key},{pid, BrokerPid}},
							error_logger:error_msg("amqp_consume_manager: Exit exception while attempting to reject message ~p from broker ~p: ~p~n",[MessageId, BrokerPid, EXIT])
					end;
				Subscription ->
					pe_delivery_pool:deliver(Subscription, Content, Secret, [{broker,BrokerPid}, {delivery_tag, DeliveryTag}])
			end
	end,
	{noreply, State};

handle_info(Msg, State) ->
	error_logger:info_msg("amqp_consume_manager: Unknown handle_info: ~p~n",[Msg]),
	{noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.




