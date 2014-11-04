% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_auth).

-include("include/prospero.hrl").

-export([authenticate/4, authenticate/5, make_token/4, parse_authorization/2]).

parse_authorization(Token, Delim) ->
  case string:tokens(Token,Delim) of
    [PrincipalId,Date,Signature] ->
      {ok, PrincipalId, Date, Signature};
    _Error ->
      {error, unrecognized_authorization_token_format}
  end.
  
make_token(PrincipalId, Date, Signature, Delim) ->
  lists:flatten(io_lib:format("~s~s~s~s~s",[PrincipalId, Delim, Date, Delim, Signature])).
  
is_date_recent_enough(DateTimestamp, Timeout) ->
  Current = pe_time:current_timestamp(),
  
  case (DateTimestamp > Current - Timeout) andalso (DateTimestamp < Current + Timeout) of
    true ->
      true;
    _OLD ->
      {false, pe_time:format_8601(Current)}
  end.
  
authenticate(AuthToken, Payload, PrincipalLookup, Delim) ->
  authenticate(AuthToken, Payload, PrincipalLookup, Delim, pe_config:get(auth,token_date_threshold_milliseconds,10000)).

authenticate(AuthToken, Payload, PrincipalLookup, Delim, Timeout) ->
  case parse_authorization(AuthToken, Delim) of
    {ok, PrincipalId, Date, Signature} ->
      
      case pe_time:parse_8601(Date) of
        {ok, DateTimestamp} ->     
          case is_date_recent_enough(DateTimestamp, Timeout) of
            true ->
              case PrincipalLookup:lookup(PrincipalId) of
                {found, Principal} ->
                  case pe_principal:get(date_deactivated, Principal) of
                    undefined ->
                      SecretKey = pe_principal:get(secret, Principal),
                      case pe_util:verify_signature(Payload, SecretKey, Signature, Date) of
                        {valid} ->
                          {authenticated_ok, PrincipalId};
                        {invalid, {expected, _E}, {received, _R}} ->
                          {not_authenticated, "Invalid authorization signature"}
                          %CKC - This alternative return value can be helpful for debugging, but we do not want to return the expected signature and key to a real user/hacker!!!
                          %TmpExpected = [{payload, hex:bin_to_hexstr(pe_util:to_binary(Payload))},{expected, _E},{received, _R},{date, Date},{key, SecretKey},{signature, Signature}],
                          %{not_authenticated, lists:flatten(io_lib:format("Invalid authorization signature: ~p ~p",[PrincipalId, TmpExpected]))}
                      end;
                    _INACTIVE ->
                      {not_authenticated, lists:flatten(io_lib:format("Principal ~s is not active",[PrincipalId]))}
                  end;
                none ->
                  {not_authenticated, lists:flatten(io_lib:format("Principal ~s does not exist",[PrincipalId]))}
              end;
            {false, _CurrentTime} ->
              {not_authenticated, lists:flatten(io_lib:format("Date from authorization token has expired: ~s (Timeout is ~p milliseconds)", [Date, Timeout]))}
          end;                     
        {error, Error} ->
          {not_authenticated, lists:flatten(io_lib:format("Unable to parse authorization date: ~p (DateString: ~s)", [Error, Date]))}         
      end; 
    {error, Error} ->
      {not_authenticated, lists:flatten(io_lib:format("Unable to parse authorization token: ~p (Token: ~s, Delim: ~s)", [Error, AuthToken, Delim]))}
  end.

