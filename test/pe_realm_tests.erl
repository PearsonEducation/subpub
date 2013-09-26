-module(pe_realm_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


is_within_realm_test() ->
  ?assert(pe_realm:is_within_realm(".you",".you") == ok),
  ?assert(pe_realm:is_within_realm("me.you","*.you") == ok),
  ?assert(pe_realm:is_within_realm("*me.you","*.you") == ok),
  ?assert(pe_realm:is_within_realm("*",".you") == ok),
  ?assert(pe_realm:is_within_realm("me.you","*") == ok),
  ?assert(pe_realm:is_within_realm("me.you","*.your") == not_within),
  ?assert(pe_realm:is_within_realm("me.you",".you") == not_within).
