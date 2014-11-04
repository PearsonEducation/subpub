% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(amqp_managed_connection).

-define(AC_STATUS_UNINITIALIZED,"uninitialized").
-define(AC_STATUS_CONNECTING,"connecting").
-define(AC_STATUS_CONNECTED,"connected").
-define(AC_STATUS_NOT_CONNECTED,"not_connected").

-define(DEFAULT_RECONNECT_WAIT_MILLIS, 1000).
-define(DEFAULT_PREFETCH_COUNT, 10).

-define(DEFAULT_RAMPDOWN_INTERVAL_SECONDS, 0).
-define(DEFAULT_RAMPDOWN_MULTIPLIER, 1).
-define(DEFAULT_RAMPDOWN_WAIT_MILLIS, 1).
-define(DEFAULT_CONFIRMED_PUBLISHING, false).

-define(AC_NO_PID,none).

-record(ac_state, {
  linked_pid=?AC_NO_PID,
  params,
  key,
  connection,
  channel,
  confirmed_channel,
  exchange,
  consumed_queue=undefined,
  consumer_realm,
  consumer_secret,
  sub_id,
  num_consecutive_connect_attempts=0,
  channel_message_count=0,
  confirmed_channel_message_count=0,
  is_close_requested=false,
  status=?AC_STATUS_UNINITIALIZED,
  handlers=[{connected,[]},{disconnected,[]},{message,[]},{publish_return,[]}],
  is_confirmed_publishing,
  in_flight_messages,
  dog_tag=undefined
}).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
  connect/1, 
  close/1, 
  publish_headers/6, 
  register_handler/2, 
  declare_queue/3, 
  consume_queue/5, 
  ack/2, 
  reject/2, 
  reject/3, 
  delete_queue/1, 
  set_dog_tag/2, 
  get_dog_tag/1
]).

-include_lib("amqp_client/include/amqp_client.hrl").
-include("include/prospero.hrl").

%% START API %%
connect(Pid) ->
  gen_server:call(Pid, connect).

close(Pid) ->
  gen_server:call(Pid, close).

publish_headers(Pid, Headers, Payload, MessageId, TimeoutMillis, ReplyTag) ->
  gen_server:call(Pid, {publish_headers, Payload, Headers, MessageId, TimeoutMillis, ReplyTag}).

register_handler(Pid, Type) ->
  gen_server:cast(Pid, {register, Type, self()}).

declare_queue(Pid, Name, Headers) ->
  gen_server:call(Pid, {declare_queue, Name, Headers}).

consume_queue(Pid, Name, Realm, SubId, Secret) ->
  gen_server:call(Pid, {consume_queue, Name, Realm, Secret, SubId}).

ack(Pid, DeliveryTag) ->
  gen_server:cast(Pid, {ack, DeliveryTag}).

reject(Pid, DeliveryTag, RequeueNewPayload) ->
  gen_server:cast(Pid, {reject, DeliveryTag, RequeueNewPayload}).

reject(Pid, DeliveryTag) ->
  gen_server:cast(Pid, {reject, DeliveryTag}).

delete_queue(Pid) ->
  gen_server:call(Pid, delete_queue).

set_dog_tag(Pid, DogTag) ->
  gen_server:call(Pid, {set_dog_tag, DogTag}).

get_dog_tag(Pid) ->
  gen_server:call(Pid, get_dog_tag).

%% END API %%

init([Params, Exchange, Key]) ->
  {ok, #ac_state{params=Params, key=Key, exchange=list_to_binary(Exchange), in_flight_messages=gb_trees:empty()}}.

notify(Type, #ac_state{key=Key} = State) ->
  do_notify(Type, {Type,{key, Key},{pid, self()}}, State).

notify(Type, Data, #ac_state{key=Key} = State) ->
  do_notify(Type, {Type,{key, Key},{pid, self()},Data}, State).

do_notify(Type, Message, #ac_state{handlers=Handlers}) ->
  HandlersForType = proplists:get_value(Type, Handlers, []),
  lists:foreach(fun(E) -> E ! Message end, HandlersForType).

trigger_connection_attempt(NumFailedAttempts) ->
  WaitTime = pe_config:get(amqp, reconnect_wait_millis, ?DEFAULT_RECONNECT_WAIT_MILLIS) * NumFailedAttempts,
  erlang:send_after( WaitTime, self(), attempt_connect_then_notify).

handle_call(get_dog_tag, _From, State) ->
  {reply, {ok, State#ac_state.dog_tag}, State};
  
handle_call({set_dog_tag, DogTag}, _From, State) ->
  {reply, ok, State#ac_state{dog_tag=DogTag}};

handle_call({declare_queue, QueueName, Headers}, _From, #ac_state{channel=Channel, exchange=Exchange} = State) ->
  Headers2 = lists:append([table_entry("x-match","all", longstr)],headers_list_to_amqp_table(Headers)),
  QueueDeclare = #'queue.declare'{queue = list_to_binary(QueueName), durable = true},
  #'queue.declare_ok'{} = amqp_channel:call(Channel, QueueDeclare),
 
  QueueBind1 = #'queue.bind'{queue = list_to_binary(QueueName), exchange = Exchange, arguments = Headers2},
  #'queue.bind_ok'{} = amqp_channel:call(Channel, QueueBind1),
  
  %There is already a default binding on the default exchange with the queue name as the routing key, so it is not necessary to create one.
  %QueueBind2 = #'queue.bind'{queue = QueueName, exchange = get_exchange_from_info(ConnInfo), routing_key = QueueName}, %% We bind the queue a second time with a unique routing key so that we can put a message directly into the queue. %%
  %#'queue.bind_ok'{} = amqp_channel:call(get_channel_from_info(ConnInfo), QueueBind2),
  {reply, ok, State};

handle_call(delete_queue, _From, #ac_state{channel=Channel, consumed_queue=QueueName} = State) ->
  case QueueName of
    undefined ->
      ok;
    _EXISTS ->
      try
        QueueDelete = #'queue.delete'{queue = pe_util:to_binary(QueueName), if_unused = false, if_empty = false, nowait = true},
        amqp_channel:call(Channel, QueueDelete),
        ok
      catch
        exit:{{server_initiated_close,404,_MSG}, _} -> queue_does_not_exist_reconnect_necessary;
        throw:{{server_initiated_close,404,_MSG}, _} -> queue_does_not_exist_reconnect_necessary;
        error:{{server_initiated_close,404,_MSG}, _} -> queue_does_not_exist_reconnect_necessary
      end
  end,
  {reply, ok, State};

handle_call({consume_queue, QueueName, Realm, Secret, SubId}, _From, #ac_state{channel=Channel} = State) ->
  QueueNameBin = pe_util:to_binary(QueueName),
  BasicConsume = #'basic.consume'{queue = QueueNameBin, consumer_tag = <<"">>, no_ack = false},
  #'basic.consume_ok'{} = amqp_channel:subscribe(Channel, BasicConsume, self()),
  {reply, ok, State#ac_state{sub_id=SubId, consumed_queue=QueueNameBin, consumer_secret=Secret, consumer_realm=Realm}};

handle_call(close, _From, #ac_state{connection=Connection} = State) ->
  NewState = case State#ac_state.status of
    ?AC_STATUS_CONNECTING ->
      State#ac_state{is_close_requested=true}; %Will short-circuit a queued attempt_connect_then_notify call
    ?AC_STATUS_CONNECTED ->
      try
        amqp_connection:close(Connection),
        State
      catch
        exit:EXIT ->
          error_logger:error_msg("amqp_connection: Exit exception while attempting to close: ~p~n",[EXIT]),
          State
      end;
    _ELSE ->
      State
  end,
  {reply, ok, NewState};
  
handle_call(connect, _From, State) ->
  case State#ac_state.status of
    ?AC_STATUS_CONNECTING ->
      {reply, already_connecting, State};
    ?AC_STATUS_CONNECTED ->
      {reply, already_connected, State};
    _ELSE ->
      trigger_connection_attempt(0),
      {reply, ok, State#ac_state{status=?AC_STATUS_CONNECTING, num_consecutive_connect_attempts=0, is_close_requested=false}}
  end;

handle_call({publish_headers, Payload, Headers, MessageId, TimeoutMillis, ReplyTag}, From, #ac_state{exchange=Exchange, confirmed_channel=undefined, connection=Connection} = State) ->
  pe_audit:log([],"In confirmed publish_headers"),
  ConfirmedChannel = case create_channel({Connection, Exchange}, true) of
        {ok, ConfChannel} -> ConfChannel;
        {error, CONFIRMED_CHANNEL_ERROR} ->
          error_logger:error_msg("amqp_connection: Aborting new connection for for confirmed_channel: Unable to open channel: ~p~n", [CONFIRMED_CHANNEL_ERROR]),
          undefined
        end,
  NewState = State#ac_state{confirmed_channel=ConfirmedChannel},
  handle_call({publish_headers, Payload, Headers, MessageId, TimeoutMillis, ReplyTag}, From, NewState);
handle_call({publish_headers, Payload, Headers, MessageId, _TimeoutMillis, {_PubType, _Pid, {{_From, IsDurable}, _Input}} = ReplyTag}, {FromPid, _Tag}, #ac_state{is_confirmed_publishing=_IsConfirmedPublishing, in_flight_messages=IFMessages, exchange=Exchange, channel=Channel, confirmed_channel=ConfirmedChannel, connection=_Connection} = State) ->
  pe_audit:log([],"In un-confirmed publish_headers"),
	%NOTE (CKC 3/7/12):  TimeoutMillis can be used to set a timer and return a publish_wait_timeout response but that is not yet implemented.
	case State#ac_state.status of
		?AC_STATUS_CONNECTED ->
			Headers2 = headers_list_to_amqp_table(Headers),
			BasicPublish = #'basic.publish'{exchange = Exchange, mandatory=true},

			case IsDurable of
				true -> 
					%%%%Select Confirmed Channel and add new message to IFMessages%%%%
					%% Only add it to the in-flight messages if publish_confirms are on.  Otherwise there would be a leak because messages would never get removed.
					MessageNumber = State#ac_state.confirmed_channel_message_count + 1,
					NewIFMessages = gb_trees:insert(MessageNumber, {FromPid, MessageId, ReplyTag}, IFMessages),
					try
						ok = amqp_channel:call(ConfirmedChannel, BasicPublish, #amqp_msg{payload = Payload, props = #'P_basic'{headers = Headers2, correlation_id = MessageId, delivery_mode=2 }}),
						{reply, publish_wait_timeout, State#ac_state{confirmed_channel_message_count=MessageNumber, in_flight_messages=NewIFMessages, confirmed_channel=ConfirmedChannel}}
					catch
						exit:EXIT ->
						  error_logger:error_msg("amqp_connection: Exit exception while attempting to publish message: ~p~n",[EXIT]),
						  {reply, not_connected, State}
					end;
				_ELSE ->
					try
						MessageNumber = State#ac_state.channel_message_count + 1,
						ok = amqp_channel:call(Channel, BasicPublish, #amqp_msg{payload = Payload, props = #'P_basic'{headers = Headers2, correlation_id = MessageId, delivery_mode=2 }}),
						{reply, ok, State#ac_state{channel_message_count=MessageNumber, in_flight_messages=IFMessages}}
					catch
						exit:EXIT ->
						  error_logger:error_msg("amqp_connection: Exit exception while attempting to publish message: ~p~n",[EXIT]),
						  {reply, not_connected, State}
					end
			end;
		_ELSE ->
			{reply, not_connected, State}
	end;

handle_call(Msg, _From, State) ->
  error_logger:info_msg("amqp_connection: Unknown handle_call to key ~p: ~p~n",[State#ac_state.key, Msg]),
  {reply, {error, unknown}, State}.

handle_cast({ack, _DeliveryTag}, #ac_state{channel=undefined} = State) ->
  {noreply, State};

handle_cast({ack, DeliveryTag}, #ac_state{channel=Channel} = State) ->
  amqp_channel:call(Channel,#'basic.ack'{delivery_tag=DeliveryTag}),
  {noreply, State};

handle_cast({reject, DeliveryTag}, State) ->
  handle_cast({ack, DeliveryTag}, State);

handle_cast({reject, _DeliveryTag, _RequeueNewPayload}, #ac_state{channel=undefined} = State) ->
  {noreply, State};

handle_cast({reject, DeliveryTag, RequeueNewPayload}, #ac_state{channel=Channel, consumed_queue=QueueName} = State) ->
  BasicPublish = #'basic.publish'{routing_key = QueueName},

  MessageId = pe_util:to_binary(pe_util:deserialize_message(msg_id, RequeueNewPayload)),
  
  ok = amqp_channel:cast(Channel, BasicPublish, _MsgPayload = #amqp_msg{payload = RequeueNewPayload, props = #'P_basic'{correlation_id = MessageId, delivery_mode=2 }}),
  amqp_channel:call(Channel,#'basic.ack'{delivery_tag=DeliveryTag}),
  {noreply, State};

handle_cast({register, Type, Pid}, #ac_state{handlers=Handlers} = State) ->
  HandlersForType = proplists:get_value(Type, Handlers),
  NewHandlers = [{Type,[Pid|HandlersForType]} | proplists:delete(Type, Handlers)],
  {noreply, State#ac_state{handlers=NewHandlers}};

handle_cast(Msg, State) ->
  error_logger:info_msg("amqp_connection: Unknown handle_cast to key ~p: ~p~n",[State#ac_state.key, Msg]),
  {noreply, State}.

handle_info(attempt_connect_then_notify, #ac_state{is_close_requested=IsCloseRequested, key=Key, params=AmqpParams, exchange=Exchange} = State) ->
  case IsCloseRequested of
    false ->
      IncrementedAttemptCount = State#ac_state.num_consecutive_connect_attempts + 1,
      case amqp_connection:start(AmqpParams) of
        {ok, Connection} ->

          erlang:link(Connection),
          process_flag(trap_exit, true),

          case amqp_connection:open_channel(Connection) of
            {ok, Channel} ->
              ExchangeDeclare = #'exchange.declare'{exchange = Exchange, type = <<"headers">>, durable = true},
              #'exchange.declare_ok'{} = amqp_channel:call(Channel, ExchangeDeclare),

              BasicQos = #'basic.qos'{prefetch_size = 0, prefetch_count = pe_config:get(amqp, consumer_prefetch_count,?DEFAULT_PREFETCH_COUNT), global = false},
              #'basic.qos_ok'{} = amqp_channel:call(Channel, BasicQos),

              amqp_channel:register_return_handler(Channel, self()),
              amqp_channel:register_confirm_handler(Channel, self()),

              IsConfirmedPublishing = pe_config:get(amqp, confirmed_publishing, ?DEFAULT_CONFIRMED_PUBLISHING),

              case IsConfirmedPublishing of
                true ->
                  #'confirm.select_ok'{} = amqp_channel:call(Channel, #'confirm.select'{}),
                  ok;
                _ELSE -> ok
              end,

              notify(connected, State),
              {noreply, State#ac_state{linked_pid=Connection, channel_message_count=0, confirmed_channel_message_count=0, num_consecutive_connect_attempts=0, connection=Connection, channel=Channel, confirmed_channel=undefined, is_confirmed_publishing=IsConfirmedPublishing, status=?AC_STATUS_CONNECTED}};
              %{noreply, State#ac_state{linked_pid=Connection, channel_message_count=0, num_consecutive_connect_attempts=0, connection=Connection, channel=Channel, is_confirmed_publishing=IsConfirmedPublishing, status=?AC_STATUS_CONNECTED}};
            {error, CHANNEL_ERROR} ->
              error_logger:error_msg("amqp_connection: Aborting new connection for key ~p: Unable to open channel: ~p~n", [Key, CHANNEL_ERROR]),
              amqp_connection:close(Connection),

              trigger_connection_attempt(IncrementedAttemptCount),

              {noreply, State#ac_state{num_consecutive_connect_attempts=IncrementedAttemptCount}}
          end;
        {error, CONNECTION_ERROR} ->
          error_logger:error_msg("amqp_connection: Unable to connect for key ~p: ~p~n", [Key, CONNECTION_ERROR]),
          trigger_connection_attempt(IncrementedAttemptCount),
          {noreply, State#ac_state{num_consecutive_connect_attempts=IncrementedAttemptCount}}
      end;
    _ELSE ->
      {noreply, State}
  end;


handle_info({'EXIT',Pid, Reason}, State) ->
  ConnectionPid = State#ac_state.linked_pid,
  notify(disconnected, State),
  case Pid of
    ConnectionPid ->
      error_logger:info_msg("amqp_connection: Received EXIT from AMQP connection ~p: ~p~n",[State#ac_state.key, Reason]),
      NewState = State#ac_state{linked_pid=?AC_NO_PID, channel=undefined, connection=undefined, status=?AC_STATUS_NOT_CONNECTED, num_consecutive_connect_attempts=0},
      {noreply, NewState};
    _UNKNOWN ->
      error_logger:info_msg("amqp_connection: Received EXIT from unknown pid ~p of key ~p: ~p~n",[Pid, State#ac_state.key, Reason]),
      {noreply, State}
  end;

handle_info({#'basic.return'{reply_code=312}, #amqp_msg{props = #'P_basic'{correlation_id=MessageIdBin}}}, State) ->
  notify(publish_return, {message_id, MessageIdBin}, State),
  {noreply, State};

handle_info({'basic.ack',UpToAndIncludingNumber,IsMultiple}, #ac_state{in_flight_messages=IFMessages} = State) ->
  NewIFMessages = notify_publish_result(publish_confirm, UpToAndIncludingNumber, IsMultiple, IFMessages),
  {noreply, State#ac_state{in_flight_messages=NewIFMessages}};
          
handle_info({'basic.nack',UpToAndIncludingNumber,IsMultiple}, #ac_state{in_flight_messages=IFMessages} = State) ->
  NewIFMessages = notify_publish_result(publish_reject, UpToAndIncludingNumber, IsMultiple, IFMessages),
  error_logger:info_msg("Notifying reject for messages up to and including ~p (multiple: ~p)~n",[UpToAndIncludingNumber, IsMultiple]),
  {noreply, State#ac_state{in_flight_messages=NewIFMessages}};

handle_info({'basic.consume_ok', _Tag}, State) ->
  {noreply, State};

handle_info({'basic.cancel', _Tag, _NoWait}, State) ->
  {noreply, State};

handle_info({#'basic.deliver'{delivery_tag=DeliveryTag}, Content}, #ac_state{sub_id=SubId, consumer_secret=Secret, consumer_realm=SubscriptionRealm} = State) ->
  #amqp_msg{payload = Payload, props = Props} = Content,
  MessageId = Props#'P_basic'.correlation_id,
  MessageRealm = pe_util:get_message_realm(Payload),
  case pe_realm:is_within_realm(MessageRealm, SubscriptionRealm) of
    ok ->
      case pe_util:has_not_exceeded_max_failed_attempts(pe_config:get(delivery,max_failed_attempts,1000), Payload) of
        true ->
          case pe_util:is_message_due_for_redelivery(pe_config:get(delivery, rampdown_interval_seconds, ?DEFAULT_RAMPDOWN_INTERVAL_SECONDS), pe_config:get(delivery, rampdown_multiplier, ?DEFAULT_RAMPDOWN_MULTIPLIER), Payload) of
            true ->
              notify(message, [{message_id, MessageId},{delivery_tag, DeliveryTag},{content, Payload},{principal_secret, Secret}], State);
            false ->
              timer:sleep(pe_config:get(delivery, rampdown_wait_milliseconds, ?DEFAULT_RAMPDOWN_WAIT_MILLIS)),
              handle_cast({reject, DeliveryTag, pe_util:increment_message_rampdown_churn_count(Payload)}, State)
          end;
        {false, NumFailedDeliveries, RampDownChurnCount, TotalTimeSeconds} ->
          pe_audit:log([{sub,SubId},{msg,binary_to_list(MessageId)},{num_failed_attempts,NumFailedDeliveries},{ramp_churn,RampDownChurnCount},{total_time_seconds,TotalTimeSeconds}],"UNDELIVERABLE"),
          handle_cast({ack, DeliveryTag}, State)
      end;
    not_within ->
      pe_audit:log([{sub,SubId},{msg,binary_to_list(MessageId)},{msg_realm,MessageRealm},{principal_realm,SubscriptionRealm}],"OUTSIDE-OF-REALM"),
      handle_cast({ack, DeliveryTag}, State)
  end,
  {noreply, State};

handle_info(Msg, State) ->
  error_logger:info_msg("amqp_connection: Unknown handle_info to key ~p: ~p~n",[State#ac_state.key, Msg]),
  {noreply, State}.
  
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

notify_publish_result_recursive(Result, Current, UpTo, Tree) ->
  case Current =< UpTo of
    true ->
      NewTree = case gb_trees:lookup(Current, Tree) of
        {value, {FromPid, MessageId, ReplyTag}} ->
	  gen_server:cast(FromPid, {Result, MessageId, ReplyTag}),
          gb_trees:delete(Current, Tree);
        none ->
          Tree
      end,
      notify_publish_result_recursive(Result, Current + 1, UpTo, NewTree);
    false ->
      Tree
  end.

notify_publish_result(Result, UpToAndIncludingNumber, true = _IsMultiple, IFMessages) ->
  case gb_trees:is_empty(IFMessages) of
    true ->
      IFMessages;
    false ->
      {Smallest, _Value} = gb_trees:smallest(IFMessages),
      notify_publish_result_recursive(Result, Smallest, UpToAndIncludingNumber, IFMessages)
  end;

notify_publish_result(Result, UpToAndIncludingNumber, false = _IsMultiple, IFMessages) ->
  notify_publish_result_recursive(Result, UpToAndIncludingNumber, UpToAndIncludingNumber, IFMessages).

headers_list_to_amqp_table(Headers) ->
  headers_list_to_amqp_table(Headers, []).

headers_list_to_amqp_table([], AmqpTable) ->
  AmqpTable;

headers_list_to_amqp_table([H|T], AmqpTable) ->
  NewAmqpTableEntry = table_entry(H#pe_tag.type, H#pe_tag.value, binary),
  headers_list_to_amqp_table(T, [NewAmqpTableEntry|AmqpTable]).

table_entry(Key,Value,binary) ->
  { list_to_binary(Key),binary,list_to_binary(Value) };
table_entry(Key,Value,Type) ->
  { list_to_binary(Key),Type,Value }.

%% Resource creation helpers %%
create_channel({Connection, Exchange}, IsDurable) ->
	% Create publishing channel %
	case amqp_connection:open_channel(Connection) of
		{ok, NewChannel} -> 
			%Cross-connection params
			BasicQos = #'basic.qos'{prefetch_size = 0, prefetch_count = pe_config:get(amqp, consumer_prefetch_count,?DEFAULT_PREFETCH_COUNT), global = false},
			ExchangeDeclare = #'exchange.declare'{exchange = Exchange, type = <<"headers">>, durable = true},


			case IsDurable of
			 true ->
			    #'confirm.select_ok'{} = amqp_channel:call(NewChannel, #'confirm.select'{}),
			    ok;
			 _ELSE -> ok
			end, 

			#'exchange.declare_ok'{} = amqp_channel:call(NewChannel, ExchangeDeclare),

			#'basic.qos_ok'{} = amqp_channel:call(NewChannel, BasicQos),

			amqp_channel:register_return_handler(NewChannel, self()),
			amqp_channel:register_confirm_handler(NewChannel, self()),

			{ok, NewChannel};
		{error, CHANNEL_ERROR} -> {error, CHANNEL_ERROR}
	end.
