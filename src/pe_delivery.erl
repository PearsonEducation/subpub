% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_delivery).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
	deliver/5,
	list_in_flight_deliveries/1
]).

-define(DEFAULT_IBROWSE_MAX_SESSIONS, 10).
-define(DEFAULT_IBROWSE_MAX_PIPELINE_SIZE, 10).
-define(DEFAULT_DELIVERY_TIMEOUT_MILLIS, 30000).

-record(ped_state, {
	in_flight_deliveries=[]
}).

%% START API%%
deliver(Pid, Subscription, Message, Secret, From) ->
	gen_server:cast(Pid, {deliver, Subscription, Message, Secret, From}).

list_in_flight_deliveries(Pid) ->
	gen_server:call(Pid, list_if_deliveries).

%start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%% END API%%

init([]) ->
	{ok, #ped_state{}}.

gen_auth(undefined, _Payload, _PrincipalId) ->
  undefined;
gen_auth(Secret, Payload, PrincipalId) ->
  Timestamp = pe_time:format_8601(),
  Sig = pe_util:create_signature(Payload, Secret, Timestamp),
  pe_auth:make_token(PrincipalId, Timestamp, Sig, "|").
  
add_ssl_options(<<"https",_TheRest>>, Options) ->
  [{is_ssl, true}|Options];
  
add_ssl_options(_NonHttpsUrl, Options) ->
  Options.

%% This is only used for debugging...
handle_call(list_if_deliveries, _From, State) ->
	{reply, {ok, State#ped_state.in_flight_deliveries}, State};

handle_call(Msg, _From, State) ->
	error_logger:info_msg("pe_delivery: Unknown handle_call from ~p: ~p~n",[_From, Msg]),
	{reply, unknown, State}.

handle_cast({deliver, Subscription, MessageBin, Secret, From}, State) ->
    {id,DelAttemptId} = pe_id_broker:get_next(),

    TimeoutMilliseconds = pe_config:get(delivery,callback_timeout,?DEFAULT_DELIVERY_TIMEOUT_MILLIS),
    MaxSessions = pe_config:get(delivery,ibrowse_max_sessions,?DEFAULT_IBROWSE_MAX_SESSIONS),
    MaxPipelineSize = pe_config:get(delivery,ibrowse_max_pipeline_size,?DEFAULT_IBROWSE_MAX_PIPELINE_SIZE),

    SubId = pe_sub:get(id, Subscription),
    PrincipalId = pe_sub:get(principal_id, Subscription),
    Url = pe_sub:get(callback_url, Subscription),

    MessageProps = pe_util:deserialize_message(MessageBin),
    MsgId = proplists:get_value(msg_id, MessageProps),
    MessageType = proplists:get_value(message_type, MessageProps),
    PayloadContentType = proplists:get_value(payload_content_type, MessageProps),
    Payload = proplists:get_value(payload, MessageProps),
    Timestamp = proplists:get_value(timestamp, MessageProps),
    RampDownChurnCount = proplists:get_value(rampdown_churn_count, MessageProps),
    NumFailedDeliveries = proplists:get_value(num_failed_deliveries, MessageProps),

    Start = pe_time:current_timestamp(),
    
    pe_audit:log([{sub,SubId},{msg,MsgId},{del_attempt,DelAttemptId}],"ATTEMPTING-DELIVERY"),

    Tmp1 = list_to_binary(lists:flatten([MsgId,MessageType,PayloadContentType])),
    TokenData = <<Tmp1/binary,Payload/binary>>,

	AuthToken = gen_auth(Secret, TokenData, PrincipalId),
	{FormContentType, RequestBody} = case pe_util:is_string_based_content_type(PayloadContentType) of
		false ->
			{mime, Boundary, Body} = mime:encode([
				mime:mime_part(undefined,"form-data; name=\"DELIVERY-ATTEMPT-ID\"",undefined,DelAttemptId),
				mime:mime_part(undefined,"form-data; name=\"MESSAGE-TYPE\"",undefined,MessageType),
				mime:mime_part(undefined,"form-data; name=\"MESSAGE-ID\"",undefined,MsgId),
				mime:mime_part(undefined,"form-data; name=\"AUTHORIZATION\"",undefined,AuthToken),
				mime:mime_part(undefined,"form-data; name=\"AUTHORIZATION-DELIMITER\"",undefined,"|"),
				mime:mime_part(undefined,"form-data; name=\"PAYLOAD-CONTENT-TYPE\"",undefined,PayloadContentType),
				mime:mime_part("application/octet-stream","form-data; name=\"PAYLOAD\"; filename=\"payload\"",undefined,Payload)
			]),
			{"multipart/form-data; boundary=" ++ Boundary, Body};
		true ->
			Tmp = [
				{"DELIVERY-ATTEMPT-ID", DelAttemptId},
				{"MESSAGE-TYPE", MessageType},
				{"MESSAGE-ID", MsgId},
				{"AUTHORIZATION", AuthToken},
				{"AUTHORIZATION-DELIMITER", "|"},
				{"PAYLOAD", binary_to_list(Payload)},
				{"PAYLOAD-CONTENT-TYPE", PayloadContentType}
			],
			Body = mochiweb_util:urlencode(Tmp),
			{"application/x-www-form-urlencoded", Body}
	end,

	Headers = [{"Content-Type", FormContentType}],
	Options1 = [
		{inactivity_timeout, TimeoutMilliseconds},
		{connect_timeout, TimeoutMilliseconds},
		{content_type, FormContentType},
		{max_sessions, MaxSessions},
		{max_pipeline_size, MaxPipelineSize},
		{stream_to, self()}
	],
	Options = add_ssl_options(Url, Options1),

	case ibrowse:send_req(Url, Headers, post, RequestBody, Options, TimeoutMilliseconds) of
		{ibrowse_req_id,ReqId} ->
			NewIFDeliveries = [{ReqId, {Start, From, undefined, undefined, {MsgId, DelAttemptId, SubId, Timestamp, RampDownChurnCount, NumFailedDeliveries, MessageBin}}}|State#ped_state.in_flight_deliveries],
	event_manager:notify({msg_delivery_timer, SubId, pe_time:current_timestamp() - Start}),
			{noreply, State#ped_state{in_flight_deliveries=NewIFDeliveries}};
		{error, Reason} ->
            pe_audit:log([{sub,SubId},{msg,MsgId},{del_attempt,DelAttemptId},{reason,Reason},{duration_seconds,pe_time:current_timestamp() - Start},{num_failed_attempts,NumFailedDeliveries + 1}],"ERRORED"),
            event_manager:notify({delivery_failed, MsgId, SubId}),
	    	event_manager:notify({msg_delivery_timer, SubId, pe_time:current_timestamp() - Start}),
			amqp_consume_manager:reject(From, MessageBin),
			{noreply, State}
	end;	

handle_cast(Msg, State) ->
	error_logger:info_msg("pe_delivery: Unknown handle_cast: ~p~n",[Msg]),
	{noreply, State}.

handle_info({ibrowse_async_headers, ReqId, Status, _Headers} = Msg, State) ->
	IFDeliveries = State#ped_state.in_flight_deliveries,
	case proplists:get_value(ReqId, IFDeliveries, undefined) of
		undefined ->
			error_logger:error_msg("pe_delivery:  Received ibrowse_async_headers request for a request id that does not exist in the state: Msg: ~p, State: ~p~n", [Msg, State]),
			{noreply, State};
		{Start, From, _Status, Body, MessageDetails} ->
			NewIFDeliveries = [{ReqId, {Start, From, Status, Body, MessageDetails}}|proplists:delete(ReqId,IFDeliveries)],
			{noreply, State#ped_state{in_flight_deliveries=NewIFDeliveries}}
	end;

handle_info({ibrowse_async_response, ReqId, {error, Reason}} = Msg, State) ->
	IFDeliveries = State#ped_state.in_flight_deliveries,
	case proplists:get_value(ReqId, IFDeliveries, undefined) of
		undefined ->
			error_logger:error_msg("pe_delivery:  Received ibrowse_async_response request for a request id that does not exist in the state: Msg: ~p, State: ~p~n", [Msg, State]),
			{noreply, State};
		{Start, From, _Status, _Body, {MsgId, DelAttemptId, SubId, _Timestamp, _RampDownChurnCount, NumFailedDeliveries, MessageBin}} ->
            pe_audit:log([{sub,SubId},{msg,MsgId},{del_attempt,DelAttemptId},{reason,Reason},{duration_seconds,pe_time:current_timestamp() - Start},{num_failed_attempts,NumFailedDeliveries + 1}],"ERRORED"),
			amqp_consume_manager:reject(From, MessageBin),								
            event_manager:notify({delivery_failed, MsgId, SubId}),

			NewIFDeliveries = proplists:delete(ReqId,IFDeliveries),
			{noreply, State#ped_state{in_flight_deliveries=NewIFDeliveries}}
	end;

handle_info({ibrowse_async_response, ReqId, Response} = Msg, State) ->
	IFDeliveries = State#ped_state.in_flight_deliveries,
	case proplists:get_value(ReqId, IFDeliveries, undefined) of
		undefined ->
			error_logger:error_msg("pe_delivery:  Received ibrowse_async_response request for a request id that does not exist in the state: Msg: ~p, State: ~p~n", [Msg, State]),
			{noreply, State};
		{Start, From, Status, _Body, MessageDetails} ->
			NewIFDeliveries = [{ReqId, {Start, From, Status, Response, MessageDetails}}|proplists:delete(ReqId,IFDeliveries)],
			{noreply, State#ped_state{in_flight_deliveries=NewIFDeliveries}}
	end;
		
handle_info({_Ref, {error,{conn_failed,_ERROR}}}, State) ->
	%ibrowse sometimes sends this gratuitous message...
	{noreply, State};

handle_info({ibrowse_async_response_end, ReqId} = Msg, State) ->
	IFDeliveries = State#ped_state.in_flight_deliveries,
	case proplists:get_value(ReqId, IFDeliveries, undefined) of
		undefined ->
			error_logger:error_msg("pe_delivery:  Received ibrowse_async_response_end request for a request id that does not exist in the state: Msg: ~p, State: ~p~n", [Msg, State]),
			{noreply, State};
		{Start, From, Status, Body, {MsgId, DelAttemptId, SubId, Timestamp, RampDownChurnCount, NumFailedDeliveries, MessageBin}} ->
			case Status of
				"200" ->
            		pe_audit:log([{sub,SubId},{msg,MsgId},{del_attempt,DelAttemptId},{del_ref,Body},{duration_seconds,pe_time:current_timestamp() - Start},{latency_seconds,pe_time:current_timestamp() - Timestamp},{ramp_churn,RampDownChurnCount}],"DELIVERED"),			
					amqp_consume_manager:ack(From),
                    event_manager:notify({msg_delivered, MsgId, SubId});
				"201" ->
            		pe_audit:log([{sub,SubId},{msg,MsgId},{del_attempt,DelAttemptId},{del_ref,Body},{duration_seconds,pe_time:current_timestamp() - Start},{latency_seconds,pe_time:current_timestamp() - Timestamp},{ramp_churn,RampDownChurnCount}],"DELIVERED"),			
					amqp_consume_manager:ack(From),
                    event_manager:notify({msg_delivered, MsgId, SubId});
				"412" ->
				    pe_audit:log([{sub,SubId},{msg,MsgId},{del_attempt,DelAttemptId},{status,Status},{response,Body},{duration_seconds,pe_time:current_timestamp() - Start},{num_failed_attempts,NumFailedDeliveries}],"ABORTED"),
					amqp_consume_manager:ack(From),
                    event_manager:notify({delivery_failed, MsgId, SubId});
				"416" ->
                    pe_audit:log([{sub,SubId},{msg,MsgId},{del_attempt,DelAttemptId},{status,Status},{response,Body},{duration_seconds,pe_time:current_timestamp() - Start},{num_failed_attempts,NumFailedDeliveries}],"KILL-SUBSCRIPTION"),
					amqp_consume_manager:ack(From),
                    event_manager:notify({delivery_failed, MsgId, SubId}),
					pe_sub_intake:delete_async(SubId);
				_ELSE ->
                    pe_audit:log([{sub,SubId},{msg,MsgId},{del_attempt,DelAttemptId},{status,Status},{response,Body},{duration_seconds,pe_time:current_timestamp() - Start},{num_failed_attempts,NumFailedDeliveries + 1}],"FAILED"),
					amqp_consume_manager:reject(From, MessageBin),								
                    event_manager:notify({delivery_failed, MsgId, SubId})
			end,		
			NewIFDeliveries = proplists:delete(ReqId,IFDeliveries),
			{noreply, State#ped_state{in_flight_deliveries=NewIFDeliveries}}
	end;

handle_info(Msg, State) ->
	error_logger:info_msg("pe_delivery: Unknown handle_info: ~p~n",[Msg]),
	{noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

	
