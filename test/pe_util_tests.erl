-module(pe_util_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
  
to_binary_treats_undefined_with_empty_string_test() ->
  ?assertEqual(pe_util:to_binary(undefined), <<>>).
  
create_signature_test() ->
  ?assertEqual(pe_util:create_signature(<<"hello">>, "1234567890123456", "12/24"), "9aedf9b8fa31bd42e822b8ac368f7f03").

create_signature_not_binary_data_test() ->
  ?assertEqual(pe_util:create_signature("hello", "1234567890123456", "12/24"), "9aedf9b8fa31bd42e822b8ac368f7f03").

verify_signature_test() ->
  ?assertEqual(pe_util:verify_signature(<<"hello">>, "1234567890123456", "9aedf9b8fa31bd42e822b8ac368f7f03", "12/24"), {valid}),
  ?assertEqual(pe_util:verify_signature(<<"hello">>, "1234567890123456", "b586a8d93375c09f313524b3b90e40b2", "12/26"), {valid}),
  ?assertEqual(pe_util:verify_signature(<<"6bc1bee22e409f96e93d7e117393">>, "1234567890123456", "b8ad4a96fce2611aca1781ca462103e5", ""), {valid}).

to_binary_with_binary_input_test() ->
  ?assertEqual(pe_util:to_binary(<<"hello">>), <<"hello">>).

to_binary_with_list_input_test() ->
  ?assertEqual(pe_util:to_binary("hello"), <<"hello">>).
  
validate_input_normal_test() ->
  ?assertEqual(pe_util:validate_input(normal,"hello.-+"), valid),
  ?assertEqual(pe_util:validate_input(normal,"hello%"), invalid),
  ?assertEqual(pe_util:validate_input(normal,"hello$"), invalid),
  ?assertEqual(pe_util:validate_input(normal,"hello^"), invalid),
  ?assertEqual(pe_util:validate_input(normal,"hello*"), invalid),
  ?assertEqual(pe_util:validate_input(normal,",,"), invalid),
  ?assertEqual(pe_util:validate_input(normal,""), valid),
  ?assertEqual(pe_util:validate_input(normal,"text/html; charset=UTF-8"), invalid),
  ?assertEqual(pe_util:validate_input(normal,undefined), valid).

validate_input_content_type_test() ->
  ?assertEqual(pe_util:validate_input(content_type,"text/html; charset=UTF-8"), valid).

validate_input_realm_test() ->
  ?assertEqual(pe_util:validate_input(realm,"*"), valid),
  ?assertEqual(pe_util:validate_input(realm,"blah*"), valid),
  ?assertEqual(pe_util:validate_input(realm,"blah.bleep.*"), valid).

validate_input_url_test() ->
  ?assertEqual(pe_util:validate_input(url,"http://www.WhatEver.com/blah/dude?something=nothing"), valid),
  ?assertEqual(pe_util:validate_input(url,"https://www.whatever.com/blah/dude"), valid),
  ?assertEqual(pe_util:validate_input(url,"http://www.whatever.com/blah/dude?something=nothinggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggfffffffffffffffffffffffffffffffffff"), invalid),
  ?assertEqual(pe_util:validate_input(url,""), valid),
  ?assertEqual(pe_util:validate_input(url,undefined), valid).

validate_input_auth_test() ->
  ?assertEqual(pe_util:validate_input(auth,"ONE|2010-10-14T18:59:53Z|29d5ff22bfd074da28d288966d92d44e"), valid),
  ?assertEqual(pe_util:validate_input(auth,",,"), invalid),
  ?assertEqual(pe_util:validate_input(auth,"THREE*2010-10-14T19:53:38Z*3bb8bde38d8c99785abe59cd64f10901"), invalid),
  ?assertEqual(pe_util:validate_input(auth,"THREE,2010-10-14T19:53:38Z,3bb8bde38d8c99785abe59cd64f10901"), valid),
  ?assertEqual(pe_util:validate_input(auth,"UNKNOWN|12-4|29d5ff22bfd074da28d288966d92d44eBlahBlahBlah"), valid),
  ?assertEqual(pe_util:validate_input(auth,"THREE||2010-10-14T19:53:38Z||3bb8bde38d8c99785abe59cd64f10901"), valid).

validate_input_auth_delimiter_test() ->
  ?assertEqual(pe_util:validate_input(auth_delimiter,"|"), valid),
  ?assertEqual(pe_util:validate_input(auth_delimiter,","), valid),
  ?assertEqual(pe_util:validate_input(auth_delimiter,"||"), valid),
  ?assertEqual(pe_util:validate_input(auth_delimiter,":"), invalid),
  ?assertEqual(pe_util:validate_input(auth_delimiter,",,"), invalid).

validate_input_tags_test() ->
  ?assertEqual(pe_util:validate_input(tags,"Key:Value, Key: Value,Key:Value"), valid),
  ?assertEqual(pe_util:validate_input(tags,"Key:Value, Key: Value,Key:Value, "), valid),
  ?assertEqual(pe_util:validate_input(tags,"Key01:Value-1_2+2"), valid),
  ?assertEqual(pe_util:validate_input(tags,"Key:Value1.Value2, Key: Value,Key:Value"), valid),
  ?assertEqual(pe_util:validate_input(tags,"Key01:Value*"), invalid).

