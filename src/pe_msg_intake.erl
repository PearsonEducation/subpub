% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_msg_intake).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
 
-export([
	accept/7, 
	get_registry_contents/0,
	get_registry_keys/0,
	start_link/0
]).

-include("include/prospero.hrl").

-define(DEFAULT_INCLUDE_PAYLOAD, false).
-define(DEFAULT_PUBLISH_TIMEOUT, 10000). %TODO: Push this to config

-record(pemi_state, {
	in_flight_messages=[],
	registry=dict:new()
}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

accept(Payload, MessageType, Tags, PrincipalId, Realm, PayloadContentType, IsConfirmedPublishing) ->
  gen_server:call(?MODULE, {new, Payload, MessageType, Tags, PrincipalId, Realm, PayloadContentType, IsConfirmedPublishing}, ?DEFAULT_PUBLISH_TIMEOUT * 2).

%
% return a list of strings that are the keys to the registry.
%
get_registry_keys() ->
	gen_server:call(?MODULE, get_registry_keys, 2000).

%
% return a list of tuples that are the contents of the registry.
% The form of the tuples is:
%
%     { <key>, <value> }
%
% Where <key> is the key for the message and value is the message itself.
%
get_registry_contents() ->
	gen_server:call(?MODULE, get_registry_contents, 2000).

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

handle_call (get_registry_keys, _From, State) ->
	Registry = State#pemi_state.registry,
	Keys = dict:fetch_keys(Registry),
	{ reply, Keys, State };

handle_call (get_registry_contents, _From, State) ->
	{ reply, State#pemi_state.registry, State };

handle_call({new, Payload, MessageType, Tags, PrincipalId, Realm, PayloadContentType, IsConfirmedPublishing}, From, State) ->
  % Do Mnesia look-up for principal by PrincipalId provided
  {found, Principal} = pe_principal_store:lookup(PrincipalId),
  
  % Merge MessageType into key/value proplist TagRecordsTmp
  TagRecordsTmp = pe_tag_util:merge_message_type(Tags, MessageType), %a proplist of tag [{Key, Value}]
  
  % Creates pe_tag instances of each tag k/v pair
  TagRecordsTmp1 = pe_tag_util:instances(TagRecordsTmp),
        
  % Injects additional required tags if applicable
  TagRecords = pe_tag_util:inject_tags(pe_principal:get(enforced_tag_ids, Principal), TagRecordsTmp1, true),
  
  case pe_tag_util:has_all_required_tags_for_message(TagRecords) of
    ok ->    
      {id,MsgId} = pe_id_broker:get_next(),
      MsgIdBin = list_to_binary(MsgId),
      
      Timestamp = pe_time:current_timestamp(),
      
      % Make audit-log-friendly list of tags
      Tmp2 = pe_tag_util:make_tag_audit_log_tags(TagRecords),
      % Create audit-log-friendly list of tuples with message values
      Tmp3 = [{msg,MsgId},{principal,PrincipalId},{length,byte_size(Payload)},{realm,Realm},{content_type,PayloadContentType}],
      % Append payload content to Tmp3 for auditing IF the config value is set to include_payload, true
      Tmp4 = add_payload(Tmp3, Payload, PayloadContentType),
      
      % Log message to audit logs
      pe_audit:log(lists:append(Tmp4,Tmp2),"MESSAGE"),
      
      % Raise event for statsd
      event_manager:notify({msg_posted, MsgId}),
      
      % Create serialized binary
      Content = pe_util:serialize_message(MsgId, Timestamp, MessageType, 0, Payload, Realm, PayloadContentType),
      
      
      %NOTE: The pe_rest module can't really support async publishing because of the stupid {loop, Loop} construct that MochiWeb uses, so we'll just go with sync publishing until it becomes a bottleneck

      % Record message in event registry if it does not currently exist
      State2 = record_message(State, TagRecords, PayloadContentType, Payload),

      case do_attempt_publish_message({MsgIdBin, TagRecords, Content, ?DEFAULT_PUBLISH_TIMEOUT, IsConfirmedPublishing}, From, State2) of
        {ok, NewState, Response} ->
              {reply, Response, NewState};
        {error, ERROR, NewState} ->
          pe_audit:log([{msg,MsgId},{reason,ERROR}],"MESSAGE-NOT-PUBLISHED"),
          {reply, {error, ERROR}, NewState}
      end;
    {missing, Tag} ->
      {reply, {invalid_request, {missing_tag, Tag}}, State}
  end.


tag_records_to_string (TagRecords) ->
	tag_records_to_string ("",  TagRecords).

tag_records_to_string (String, []) -> String;

tag_records_to_string (String, [ Record | Tail ]) ->
	case convert_tag_record(Record) of
		"" -> 
			tag_records_to_string(String, Tail);

		Converted -> 
			tag_records_to_string(Converted ++ "|" ++ String, Tail)
	end.

%
% Filter out tag values for system, subsystem and message type.
%
% This function takes a pe_tag record and returns the ID for the 
% tag *if* it is system, subsystem or message type.  If not the 
% function returns an empty string.
%
convert_tag_record(#pe_tag{id = ID, type="System"}) -> ID;
convert_tag_record(#pe_tag{id = ID, type="SubSystem"}) -> ID;
convert_tag_record(#pe_tag{id = ID, type="MessageType"}) -> ID;
convert_tag_record(_) -> "".

%
% Convert a list of pe_tag records into a message_key record.
%
% This function takes a list of tags and parses them into a message_key
% record.
%
% Note that "unknown" tags in the list are ignored.  Known tags are 
% System, SubSystem and MessageType.
%
build_key (TagRecords) ->
	build_key(#message_key{}, TagRecords).

build_key (MessageKey, [ Record | Tail ]) ->
	case Record#pe_tag.type of
		"MessageType" ->
			NewMessageKey = MessageKey#message_key{messageType = Record#pe_tag.value};
		
		"SubSystem" ->
			NewMessageKey = MessageKey#message_key{subSystem = Record#pe_tag.value};

		"System" ->
			NewMessageKey = MessageKey#message_key{system = Record#pe_tag.value};

		 _ -> 
			NewMessageKey = MessageKey

	end,
	build_key(NewMessageKey, Tail);

build_key (MessageKey, []) ->
	MessageKey.

%
% update the registry with the message.  
%
% This function finds the entry that exists in the registry for the 
% message or creates one if no such entry exists.  The function looks
% in the supplied dictionary for an entry that corresponds to the 
% KeyString argument.  
% 
% The function will update the totalBytes and totalMessages counts
% for the message type to reflect the new message
%
% The function returns the updated version of the input Dictionary
% so that the entry now contains the updated record.
%
update_registry (Dictionary, KeyString, KeyObject, Content) ->
	case dict:find(KeyString, Dictionary) of
		error ->
			OldRecord = #registry_message {
				key = KeyObject,
				totalBytes = 0,
				totalMessages = 0
			};

		{ ok, OldRecord } -> ok
	end,
	NewRecord = OldRecord#registry_message {
		totalBytes = OldRecord#registry_message.totalBytes + byte_size(Content),
		totalMessages = OldRecord#registry_message.totalMessages + 1,
		content = Content
	},
	dict:store(KeyString, NewRecord, Dictionary).

%
% record a message in the registry.
%
% This function updates the registry portion of the gen_server's state 
% to include an entry for a new message type or updates the existing 
% entry.
%
% The function will only record non-binary messages.
%
record_message (State, TagRecords, ContentType, Content) ->
	MessageKey = build_key(TagRecords),
	SortedTagRecords = lists:keysort(2, TagRecords),
	Key = tag_records_to_string(SortedTagRecords),
	case ContentType of
		"multipart/form-data" -> 
			NewRegistry = State#pemi_state.registry;

		_ELSE ->
			NewRegistry = update_registry(State#pemi_state.registry, Key, MessageKey, Content)

	end,

	State#pemi_state{registry = NewRegistry}.


% Method to attempt publishing message to RabbitMQ
do_attempt_publish_message({MsgIdBin, TagRecords, Content, Timeout, IsConfirmedPublishing} = Input, From, State) ->
  case amqp_balanced_publisher:publish_headers(async, MsgIdBin, TagRecords, Content, Timeout, {{From, IsConfirmedPublishing}, Input})  of
    ok ->
      {ok, State, {publish_confirm, MsgIdBin}};
    publish_wait_timeout ->
      {ok, State, {publish_wait_timeout, MsgIdBin}};
    {error, no_brokers} ->
      error_logger:error_msg("pe_msg_intake: Unable to publish message, no amqp brokers are available~n",[]),  
      {error, no_available_brokers, State}
  end.
  
handle_cast({publish_confirm, MessageId, {From, _Input}}, State) ->
  do_notify({publish_confirm, MessageId}, From),
  {noreply, State};
   
handle_cast({publish_reject, MessageId, {From, _Input}}, State) ->
  do_notify({publish_reject, MessageId}, From),
  {noreply, State};
       
handle_cast(_Msg, State) ->
  error_logger:warning_msg("pe_msg_intake: Received unknown handle_cast: ~p~n",[_Msg]),  
  {noreply, State}.

do_notify(Response, {From, true} = _FOO) -> %Confirmed publishing
	{Pid, _} = From,
	Pid ! Response;

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
