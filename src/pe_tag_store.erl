% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

%This module is obsolete, and only exists for upgrading a database from the R1 format to R2.
-module(pe_tag_store).

-export([get_by_id/1, get_all_by_id/1]).

-include("include/prospero.hrl").
    
get_by_id(Id) ->
  case mnesia:dirty_match_object(pe_tag, {pe_tag, Id, '_', '_'}) of
    [Found] ->
      {found, Found};
    [] ->
      none
  end.
  
get_all_by_id(undefined) ->
  [];

get_all_by_id([]) ->
  [];

get_all_by_id(Ids) ->
  Finder = fun(Id) ->
    case get_by_id(Id) of
      {found, Found} ->
        {found, Id, Found};
      none ->
        {not_found, Id}
    end
  end,
  
  lists:map(Finder, Ids).
