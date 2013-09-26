-module(pe_auth_tests).

-compile(export_all).

-include("../include/prospero.hrl").

-include_lib("eunit/include/eunit.hrl").

lookup(PrincipalId) ->
  case PrincipalId of
    "VALID" ->
      {found, pe_principal:instance("VALID", undefined, undefined, true, "1234567890123456", undefined, "*", "*")};
    "INACTIVE" ->
      {found, pe_principal:instance("INACTIVE", undefined, undefined, true, "1234567890123456", undefined, pe_time:current_timestamp(), "*", "*")};
    _OTHER ->
      none
  end.

make_token_test() ->
  ?assert(pe_auth:make_token("P","D","S","{") == "P{D{S").
    
authenticate_valid_test() ->
  Payload = <<"Hello">>,
  Date = pe_time:format_8601(),
  Signature = pe_util:create_signature(Payload, "1234567890123456",Date),
  Result = pe_auth:authenticate("VALID|" ++ Date ++  "|" ++ Signature, Payload, pe_auth_tests, "|", 1000),
  ?assert(Result =:= {authenticated_ok, "VALID"}).

authenticate_invalid_signature_test() ->
  Payload = <<"Hello">>,
  Signature = pe_util:create_signature(Payload, "1234567890123454", "12/4"), %Create a signature with the wrong key
  ?assertEqual(pe_auth:authenticate("VALID|" ++ pe_time:format_8601() ++  "|" ++ Signature, Payload, pe_auth_tests, "|", 1000), {not_authenticated, "Invalid authorization signature"}).

authenticate_inactive_principal_test() ->
  Payload = <<"Hello">>,
  Signature = pe_util:create_signature(Payload, "1234567890123454", "12/4"), %Create a signature with the wrong key
  ?assertEqual(pe_auth:authenticate("INACTIVE|" ++ pe_time:format_8601() ++  "|" ++ Signature, Payload, pe_auth_tests, "|", 1000), {not_authenticated, "Principal INACTIVE is not active"}).

authenticate_nonexistent_principal_test() ->
  Payload = <<"Hello">>,
  ?assertEqual(pe_auth:authenticate("UNKNOWN|" ++ pe_time:format_8601() ++  "|whatever", Payload, pe_auth_tests, "|", 1000), {not_authenticated, "Principal UNKNOWN does not exist"}).

authenticate_invalid_token_test() ->
  ?assertEqual(pe_auth:authenticate("UNKNOWNwhatever", "", pe_auth_tests, "|", 1000), {not_authenticated, "Unable to parse authorization token: unrecognized_authorization_token_format (Token: UNKNOWNwhatever, Delim: |)"}).

authenticate_expired_token_test() ->
  Payload = <<"Hello">>,
  Date = "2004-12-20T04:05:03Z",
  Signature = pe_util:create_signature(Payload, "1234567890123456",Date),
  Result = pe_auth:authenticate("VALID|" ++ Date ++  "|" ++ Signature, Payload, pe_auth_tests, "|", 1000),
  ?assertEqual(Result, {not_authenticated, "Date from authorization token has expired: " ++ Date ++ " (Timeout is 1000 milliseconds)"}).

authenticate_expired_ahead_token_test() ->
  Payload = <<"Hello">>,
  Date = "2012-12-20T04:05:03Z",
  Signature = pe_util:create_signature(Payload, "1234567890123456",Date),
  Result = pe_auth:authenticate("VALID|" ++ Date ++  "|" ++ Signature, Payload, pe_auth_tests, "|", 1000),
  ?assertEqual(Result, {not_authenticated, "Date from authorization token has expired: " ++ Date ++ " (Timeout is 1000 milliseconds)"}).
