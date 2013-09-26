% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_msg_intake).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
 
-export([accept/7, start_link/0]).

-include("include/prospero.hrl").

-define(DEFAULT_INCLUDE_PAYLOAD, false).
-define(DEFAULT_PUBLISH_TIMEOUT, 10000). %TODO: Push this to config

-record(pemi_state, {
  in_flight_messages=[]
}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

accept(Payload, MessageType, Tags, PrincipalId, Realm, PayloadContentType, IsConfirmedPublishing) ->
  gen_server:call(?MODULE, {new, Payload, MessageType, Tags, PrincipalId, Realm, PayloadContentType, IsConfirmedPublishing}, ?DEFAULT_PUBLISH_TIMEOUT * 2).

init([]) ->
  {ok, #pemi_state{}}.
     
add_payload(Tags, Payload, true, true) -> %Config'd and string based content type
  lists:append(Tags, [{payload, Payload}]);
add_payload(Tags, Payload, false, true) -> %Config'd and binary content type
  lists:append(Tags, [{payload, base64:encode(Payload)}]);
add_payload(Tags, _Payload, _IsStringBasedContentType, _PayloadIncludedNotConfigd) -> %Not config'd
  Tags.
  
add_payload(Tags, Payload, PayloadContentType) ->
  add_payload(Tags, Payload, pe_util:is_string_based_content_type(PayloadContentType), pe_config:get(audit,include_payload,?DEFAULT_INCLUDE_PAYLOAD)).
  
handle_call({new, Payload, MessageType, Tags, PrincipalId, Realm, PayloadContentType, IsConfirmedPublishing}, From, State) ->
  {found, Principal} = pe_principal_store:lookup(PrincipalId),
  
  TagRecordsTmp = pe_tag_util:merge_message_type(Tags, MessageType), %a proplist of tag [{Key, Value}]
  
  TagRecordsTmp1 = pe_tag_util:instances(TagRecordsTmp),
        
  TagRecords = pe_tag_util:inject_tags(pe_principal:get(enforced_tag_ids, Principal), TagRecordsTmp1, true),
  
  case pe_tag_util:has_all_required_tags_for_message(TagRecords) of
    ok ->    
      {id,MsgId} = pe_id_broker:get_next(),
      MsgIdBin = list_to_binary(MsgId),
      
      Timestamp = pe_time:current_timestamp(),
      
      Tmp2 = pe_tag_util:make_tag_audit_log_tags(TagRecords),
      Tmp3 = [{msg,MsgId},{principal,PrincipalId},{length,byte_size(Payload)},{realm,Realm},{content_type,PayloadContentType}],
      Tmp4 = add_payload(Tmp3, Payload, PayloadContentType),
      
      pe_audit:log(lists:append(Tmp4,Tmp2),"MESSAGE"),
      
      event_manager:notify({msg_posted, MsgId}),
      
      Content = pe_util:serialize_message(MsgId, Timestamp, MessageType, 0, Payload, Realm, PayloadContentType),
      
      %NOTE: The pe_rest module can't really support async publishing because of the stupid {loop, Loop} construct that MochiWeb uses, so we'll just go with sync publishing until it becomes a bottleneck
      case do_attempt_publish_message({MsgIdBin, TagRecords, Content, ?DEFAULT_PUBLISH_TIMEOUT, IsConfirmedPublishing}, From, State) of
        {ok, NewState, Response} ->
          case IsConfirmedPublishing of
            true ->
              {noreply, NewState};
            _ELSE ->
              {reply, Response, NewState}
          end;
        {error, ERROR, NewState} ->
          pe_audit:log([{msg,MsgId},{reason,ERROR}],"MESSAGE-NOT-PUBLISHED"),
          {reply, {error, ERROR}, NewState}
      end;
    {missing, Tag} ->
      {reply, {invalid_request, {missing_tag, Tag}}, State}
  end.

do_attempt_publish_message({MsgIdBin, TagRecords, Content, Timeout, IsConfirmedPublishing} = Input, From, State) ->
  case amqp_balanced_publisher:publish_headers(async, MsgIdBin, TagRecords, Content, Timeout, {{From, IsConfirmedPublishing}, Input})  of
    ok ->
      {ok, State, {publish_confirm, MsgIdBin}};
    {error, no_brokers} ->
      error_logger:error_msg("pe_msg_intake: Unable to publish message, no amqp brokers are available~n",[]),  
      {error, no_available_brokers, State}
  end.
        
handle_cast(_Msg, State) ->
  error_logger:warning_msg("pe_msg_intake: Received unknown handle_cast: ~p~n",[_Msg]),  
  {noreply, State}.

do_notify(Response, {From, true} = _FOO) -> %Confirmed publishing
  gen_server:reply(From, Response);

do_notify(_Response, {_From, false} = _FOO) -> %Async unconfirmed publishing
  ok.

handle_info({publish_confirm, MessageId, {From, _Input}}, State) ->
  do_notify({publish_confirm, MessageId}, From),
  {noreply, State};

handle_info({publish_reject, MessageId, {From, Input}}, State) ->
  error_logger:error_msg("pe_msg_intake: Message ~p was rejected...attempting to re-publish it~n",[MessageId]),
  case do_attempt_publish_message(Input, From, State) of
    {ok, State} ->
      {noreply, State};
    {error, ERROR, State} ->
      error_logger:warning_msg("pe_msg_intake: Error while attempting to re-publish rejected message ~p: ~p~n",[MessageId, ERROR]),
      {noreply, State}
  end;

handle_info({publish_timeout, MessageId, {From, _Input}}, State) ->
  do_notify({publish_timeout, MessageId}, From),
  {noreply, State};

handle_info(_Msg, State) ->
  error_logger:warning_msg("pe_msg_intake: Received unknown handle_info: ~p~n",[_Msg]),  
  {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
