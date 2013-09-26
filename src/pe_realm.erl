% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_realm).

-export([validate_realm_string/1, is_within_realm/2]).

validate_realm_string(undefined) ->
  {invalid, undefined};
validate_realm_string("") ->
  {invalid, empty};
validate_realm_string("*") ->
  {valid};
validate_realm_string(Realm) ->
  case re:run(Realm,"^[0-9a-zA-Z.*\-]{1,}$") of
    {match, _Capture} ->
      case re:run(Realm,"^[*]{0,1}[0-9a-zA-Z.\-]{1,}$") of
        {match, _Capture} ->
          {valid};
        nomatch ->
          {invalid, inner_wildcards_not_allowed}
      end;
    nomatch ->
      {invalid, domain_illegal_characters}
  end.


is_within_realm("*", _RealmString) ->
  ok;
  
is_within_realm("*" ++ String, RealmString) ->
  is_within_realm(String, RealmString);

is_within_realm(String, "*" ++ RealmString) ->
  Regex = lists:flatten(io_lib:format("~s$",[RealmString])),
  case re:run(String, Regex) of
    {match, _Capture} ->
      ok;
    nomatch ->
      not_within
  end;
  
is_within_realm(String, RealmString) ->
  Regex = lists:flatten(io_lib:format("^~s$",[RealmString])),
  case re:run(String, Regex) of
    {match, _Capture} ->
      ok;
    nomatch ->
      not_within
  end.



