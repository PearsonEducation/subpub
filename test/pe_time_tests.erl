-module(pe_time_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


format_8601_test() ->
  ?assert(pe_time:format_8601(63439889195) == "2010-04-30T23:26:35Z").
  
format_8601_undefined_timestamp_test() ->
  ?assert(pe_time:format_8601(undefined) == "").
  
parse_8601_test() ->
  ?assert(pe_time:parse_8601("2010-04-30T23:26:35Z") == {ok, 63439889195}),
  ?assert(pe_time:parse_8601("2010-04-30T23:26:35+0400") == {ok, 63439903595}),
  ?assert(pe_time:parse_8601("2010-04-30T23:26:35+1000") == {ok, 63439925195}),
  ?assert(pe_time:parse_8601("2010-04-30T23:26:35-1000") == {ok, 63439853195}),
  ?assert(pe_time:parse_8601("2010-04-30T23:26:35-00:30") == {ok, 63439887395}),
  ?assert(pe_time:parse_8601("2010-04-30T23:26:35+00:30") == {ok, 63439890995}),
  ?assert(pe_time:parse_8601("2010-04-30T23:26:35-1030") == {ok, 63439851395}).

