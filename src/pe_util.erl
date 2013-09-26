% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_util).

-export([
  to_binary/1, 
  create_signature/3, 
  verify_signature/4, 
  generate_key/0, 
  serialize_message/7,
  serialize_message/8,
  serialize_message/9,
  increment_message_rampdown_churn_count/1,
  increment_failed_attempt_count_and_set_last_attempted_timestamp/1,
  is_message_due_for_redelivery/3,
  has_not_exceeded_max_failed_attempts/2,
  deserialize_message/1,
  deserialize_message/2,
  get_message_realm/1,
  empty_2_undefined/1,
  undefined_2_empty/1,
  is_string_based_content_type/1,
  validate_payload_content_type/1,
  validate_input/2,
  validate_all_inputs/1
]).
 
empty_2_undefined("") ->
  undefined;

empty_2_undefined(undefined) ->
  undefined;
 
empty_2_undefined(Val) ->
  Val.
  
undefined_2_empty(undefined) ->
  "";
undefined_2_empty(Val) ->
  Val.
  
is_string_based_content_type("application/json") -> true;
is_string_based_content_type("application/xjson") -> true;
is_string_based_content_type("application/javascript") -> true;
is_string_based_content_type("application/x-javascript") -> true;
is_string_based_content_type("application/jsonp") -> true;
is_string_based_content_type("text/json") -> true;
is_string_based_content_type("text/javascript") -> true;
is_string_based_content_type("text/x-json") -> true;
is_string_based_content_type("text/x-javascript") -> true;
is_string_based_content_type("text/plain") -> true;
is_string_based_content_type("application/xml") -> true;
is_string_based_content_type("text/xml") -> true;
is_string_based_content_type("text/html") -> true;
is_string_based_content_type(_) -> false.
  


validate_payload_content_type("application/octet-stream") -> valid;
validate_payload_content_type("application/x-protobuf") -> valid;
validate_payload_content_type("application/json") -> valid;
validate_payload_content_type("application/x-json") -> valid;
validate_payload_content_type("text/json") -> valid;
validate_payload_content_type("text/x-json") -> valid;
validate_payload_content_type("text/javascript") -> valid;
validate_payload_content_type("text/x-javascript") -> valid;
validate_payload_content_type("application/x-javascript") -> valid;
validate_payload_content_type("application/javascript") -> valid;
validate_payload_content_type("text/plain") -> valid;
validate_payload_content_type("text/xml") -> valid;
validate_payload_content_type("application/xml") -> valid;
validate_payload_content_type("text/html") -> valid;
validate_payload_content_type("application/jsonp") -> valid;
validate_payload_content_type(_) -> invalid.
  
to_binary(undefined) ->
  to_binary("");
to_binary(String) when is_list(String) ->
  list_to_binary(String);
to_binary(Arg) when is_binary(Arg) ->
  Arg.

hex(Binary) ->
  hex:bin_to_hexstr(Binary).
  
%unhex(HexString) ->
%  hex:hexstr_to_bin(HexString).

generate_key() ->
  hex(crypto:rand_bytes(16)).
  
create_signature(Data, Key, Date) ->
  BinDate = to_binary(Date),
  BinData = to_binary(Data),
  WhatToSign = <<BinDate/binary,BinData/binary>>,
  %Digest = crypto:md5(WhatToSign),
  %Digest = sha2:hexdigest256(WhatToSign),
  %IV = <<0:128>>,
  %Signed = crypto:aes_cbc_128_encrypt(Key, IV, Digest),
  %Signed = cmac_aes_cbc_128(Key, WhatToSign),
  Signed = omac1:generate_tag_aes_cbc_128(Key, WhatToSign),
  hex(Signed).
  
verify_signature(Data, Key, Signature, Date) ->
  ExpectedSig = create_signature(Data, Key, Date),
  case Signature =:= ExpectedSig of
    true ->
      {valid};
    false ->
      {invalid, {expected, ExpectedSig}, {received, Signature}}
  end.

serialize_message(MsgId, Timestamp, MessageType, NumFailedDeliveries, Payload, Realm, PayloadContentType) ->
  serialize_message(MsgId, Timestamp, MessageType, NumFailedDeliveries, Payload, Realm, PayloadContentType, pe_time:current_timestamp(), 0).
  
serialize_message(MsgId, Timestamp, MessageType, NumFailedDeliveries, Payload, Realm, PayloadContentType, RampDownChurnCount) ->
  serialize_message(MsgId, Timestamp, MessageType, NumFailedDeliveries, Payload, Realm, PayloadContentType, pe_time:current_timestamp(), RampDownChurnCount).

serialize_message(MsgId, Timestamp, MessageType, NumFailedDeliveries, Payload, Realm, PayloadContentType, LastAttemptTimestamp, RampDownChurnCount) ->  
  MsgIdBin = list_to_binary(MsgId),
  TimestampBin = list_to_binary(io_lib:format("~16..0B",[Timestamp])),
  PaddedMessageType = list_to_binary(io_lib:format("~100.. s",[MessageType])),
  NumFailedDeliveriesBin = list_to_binary(io_lib:format("~8..0B",[NumFailedDeliveries])),
  RealmBin = list_to_binary(io_lib:format("~100.. s",[Realm])),
  PayloadContentTypeBin = list_to_binary(io_lib:format("~100.. s",[PayloadContentType])),
  LastAttemptTimestampBin = list_to_binary(io_lib:format("~16..0B",[LastAttemptTimestamp])),
  RampDownChurnCountBin = list_to_binary(io_lib:format("~16..0B",[RampDownChurnCount])),
          
  <<MsgIdBin:36/binary,NumFailedDeliveriesBin:8/binary,RampDownChurnCountBin:16/binary,PaddedMessageType:100/binary,TimestampBin:16/binary,LastAttemptTimestampBin:16/binary,RealmBin:100/binary,PayloadContentTypeBin:100/binary,Payload/binary>>.

deserialize_message(<<MsgIdBin:36/binary,NumFailedDeliveriesBin:8/binary,RampDownChurnCountBin:16/binary,PaddedMessageType:100/binary,TimestampBin:16/binary,LastAttemptTimestampBin:16/binary,PaddedRealm:100/binary,PaddedPayloadContentType:100/binary,Payload/binary>>) ->
  [
    {msg_id, binary_to_list(MsgIdBin)},
    {num_failed_deliveries, list_to_integer(binary_to_list(NumFailedDeliveriesBin))},
    {rampdown_churn_count, list_to_integer(binary_to_list(RampDownChurnCountBin))},
    {message_type, string:strip(binary_to_list(PaddedMessageType))},
    {timestamp, list_to_integer(binary_to_list(TimestampBin))},
    {last_attempt_timestamp, list_to_integer(binary_to_list(LastAttemptTimestampBin))},
    {realm, string:strip(binary_to_list(PaddedRealm))},
    {payload_content_type, string:strip(binary_to_list(PaddedPayloadContentType))},
    {payload, Payload}
  ].

deserialize_message(Field, Message) ->
  proplists:get_value(Field, deserialize_message(Message)).

increment_message_rampdown_churn_count(<<MsgId:36/binary, NumFailedDeliveries:8/binary, RampDownChurnCount:16/binary, PaddedMessageType:100/binary, Timestamp:16/binary, LastAttemptTimestamp:16/binary, PaddedRealm:100/binary, PaddedPayloadContentType:100/binary, Payload/binary>>) ->
  NewRampDownChurnCount = list_to_integer(binary_to_list(RampDownChurnCount)) + 1,
  NewRampDownChurnCount2 = list_to_binary(io_lib:format("~16..0B",[NewRampDownChurnCount])),
  <<MsgId:36/binary,NumFailedDeliveries:8/binary,NewRampDownChurnCount2:16/binary,PaddedMessageType:100/binary,Timestamp:16/binary,LastAttemptTimestamp:16/binary,PaddedRealm:100/binary,PaddedPayloadContentType:100/binary,Payload/binary>>.

increment_failed_attempt_count_and_set_last_attempted_timestamp(<<MsgId:36/binary, NumFailedDeliveries:8/binary, RampDownChurnCount:16/binary, PaddedMessageType:100/binary, Timestamp:16/binary, _LastAttemptTimestamp:16/binary, PaddedRealm:100/binary, PaddedPayloadContentType:100/binary, Payload/binary>>) ->
  NewNumFailedDeliveries = list_to_integer(binary_to_list(NumFailedDeliveries)) + 1,
  NewNumFailedDeliveries2 = list_to_binary(io_lib:format("~8..0B",[NewNumFailedDeliveries])),
  LastAttemptTimestamp = list_to_binary(io_lib:format("~16..0B",[pe_time:current_timestamp()])),
  <<MsgId:36/binary,NewNumFailedDeliveries2:8/binary,RampDownChurnCount:16/binary,PaddedMessageType:100/binary,Timestamp:16/binary,LastAttemptTimestamp:16/binary,PaddedRealm:100/binary,PaddedPayloadContentType:100/binary,Payload/binary>>.

is_message_due_for_redelivery(RampdownInterval, RampdownMultiplier, <<_MsgId:36/binary, NumFailedDeliveriesBin:8/binary, _RampDownChurnCount:16/binary, _PaddedMessageType:100/binary, _Timestamp:16/binary, LastAttemptTimestampBin:16/binary, _PaddedRealm:100/binary, _PaddedPayloadContentType:100/binary, _Payload/binary>>) ->
  NumFailedDeliveries = list_to_integer(binary_to_list(NumFailedDeliveriesBin)),
  LastAttemptTimestamp = list_to_integer(binary_to_list(LastAttemptTimestampBin)),
  case NumFailedDeliveries of
    0 ->
      true;
    _ELSE ->
      DontDeliverUntil = LastAttemptTimestamp + (NumFailedDeliveries * RampdownInterval * RampdownMultiplier),
      pe_time:current_timestamp() >= DontDeliverUntil
  end.

has_not_exceeded_max_failed_attempts(MaxNumAttempts, <<_MsgId:36/binary, NumFailedDeliveriesBin:8/binary, RampDownChurnCountBin:16/binary, _PaddedMessageType:100/binary, TimestampBin:16/binary, _LastAttemptTimestampBin:16/binary, _PaddedRealm:100/binary, _PaddedPayloadContentType:100/binary, _Payload/binary>>) ->
  NumFailedDeliveries = list_to_integer(binary_to_list(NumFailedDeliveriesBin)),
  Timestamp = list_to_integer(binary_to_list(TimestampBin)),
  RampDownChurnCount = list_to_integer(binary_to_list(RampDownChurnCountBin)),

  case MaxNumAttempts > NumFailedDeliveries of
    true ->
      true;
    false ->
      {false, NumFailedDeliveries, RampDownChurnCount, pe_time:current_timestamp() - Timestamp}
  end.

get_message_realm(<<_MsgId:36/binary, _NumFailedDeliveriesBin:8/binary, _RampDownChurnCount:16/binary, _PaddedMessageType:100/binary, _Timestamp:16/binary, _LastAttemptTimestampBin:16/binary, PaddedRealm:100/binary, _PaddedPayloadContentType:100/binary, _Payload/binary>>) ->
  string:strip(binary_to_list(PaddedRealm)).

validate_input(_AnyType, undefined) ->
  valid;

validate_input(_AnyType, "") ->
  valid;

validate_input(client, Input) ->
   case re:run(Input, "^(([a-zA-Z0-9]{1,50}):( )?([a-zA-Z0-9\\.\\-\\_\\+]{1,50}))$") of
    {match,_Match} ->
     valid;
    _ELSE -> 
     invalid
  end;

validate_input(client_string, Input) ->
   case re:run(Input, "^(([a-zA-Z0-9]{1,50}):( )?([a-zA-Z0-9\\.\\-\\_\\+]{1,50}))$") of
    {match,_Match} ->
     valid;
    _ELSE -> 
     invalid
  end;

validate_input(system, Input) ->
   case re:run(Input, "^(([a-zA-Z0-9]{1,50}):( )?([a-zA-Z0-9\\.\\-\\_\\+]{1,50}))$") of
    {match,_Match} ->
     valid;
    _ELSE -> 
     invalid
  end;

validate_input(sub_system, Input) ->
   case re:run(Input, "^(([a-zA-Z0-9]{1,50}):( )?([a-zA-Z0-9\\.\\-\\_\\+]{1,50}))$") of
    {match,_Match} ->
     valid;
    _ELSE -> 
     invalid
  end;


validate_input(auth, Input) ->
  case re:run(Input, "^([0-9a-zA-Z\\,\\.\\:\\-\\+\\|]{55,200})$") of
    {match,_Match} ->
     valid;
    _ELSE -> invalid
  end;

validate_input(auth_delimiter, "|") ->
  valid;
validate_input(auth_delimiter, ",") ->
  valid;
validate_input(auth_delimiter, "||") ->
  valid;
validate_input(auth_delimiter, _Input) ->
  invalid;

validate_input(tags, Input) ->
  ParsedTags = string:tokens(Input, ", "),
  ValidationList = validate_tags(ParsedTags),
  case proplists:is_defined(invalid, ValidationList) of
    true ->
     invalid;
    false -> 
     valid
  end;

validate_input(url, Input) when length(Input) < 201 ->
  case re:run(Input, "^((https?)://)[A-Za-z0-9-]+(\.[A-Za-z0-9-]+)+([/?].*)?$") of
    {match,_Match} ->
     valid;
    _ELSE -> invalid
  end;

validate_input(url, Input) when length(Input) > 200 ->
  invalid;

validate_input(normal, Input) ->
  case re:run(Input, "^([0-9a-zA-Z\\:\\.\\+\\-]{1,100})$") of
    {match,_Match} -> valid;
    _ELSE -> invalid
  end;

validate_input(content_type, Input) ->
  case re:run(Input, "^([0-9a-zA-Z\\.\\+\\-\\/\\;\\= ]{1,100})$") of
    {match,_Match} -> valid;
    _ELSE -> invalid
  end;

validate_input(realm, Input) ->
  case re:run(Input, "^([0-9a-zA-Z\\.\\*]{1,100})$") of
    {match,_Match} -> valid;
    _ELSE -> invalid
  end.

validate_tags(Tags) ->
  validate_tags(Tags, []).

validate_tags([], TagList) ->
  TagList;
validate_tags([Tag|T], TagList) ->
  case re:run(Tag, "^(([a-zA-Z0-9]{1,50}):( )?([a-zA-Z0-9\\.\\-\\_\\+]{1,50}))$") of
    {match,_Match} ->
     [{valid,Tag}|validate_tags(T, TagList)];
    _ELSE -> 
     [{invalid,Tag}|validate_tags(T, TagList)]
  end.

validate_all_inputs([]) ->
  valid;  

validate_all_inputs([{Input, Description, Type}|TheRest]) ->
  case validate_input(Type, Input) of
    valid ->
      validate_all_inputs(TheRest);
    invalid ->
      {invalid, Description}
  end.
  


