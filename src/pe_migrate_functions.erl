% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_migrate_functions).

-export([
  convert_principal_enforced_tag_guids_to_keys/0,
  convert_subscription_tag_guids_to_keys/0
]).

-include("include/prospero.hrl").

convert_principal_enforced_tag_guids_to_keys() ->
  convert_principal_enforced_tag_guids_to_keys(pe_principal_store:no_gen_server_lookup_all()).
  
convert_principal_enforced_tag_guids_to_keys([]) ->
  ok;
  
convert_principal_enforced_tag_guids_to_keys([PrincipalId|PrincipalIds]) ->
  case pe_principal_store:no_gen_server_lookup(PrincipalId) of
    {found, Principal} ->
      OldIds = Principal#pe_principal.enforced_tag_ids,
      NewIds = convert_tag_guids_to_keys(OldIds),
      UpdatedPrincipal = Principal#pe_principal{enforced_tag_ids=NewIds},
      mnesia:dirty_write(UpdatedPrincipal),
      error_logger:info_msg("Converted enforced_tag_ids for Principal ~p: ~p -> ~p~n",[PrincipalId, OldIds, NewIds]),
      convert_principal_enforced_tag_guids_to_keys(PrincipalIds);
    _NOT_FOUND ->
      {error, {cannot_convert_principal_enforced_tag_guids_to_keys, {principal_not_found, PrincipalId}}}
  end.
  
  
  
convert_subscription_tag_guids_to_keys() ->
  convert_subscription_tag_guids_to_keys(pe_sub_store:no_gen_server_lookup_all()).

convert_subscription_tag_guids_to_keys([]) ->
  ok;

convert_subscription_tag_guids_to_keys([SubId|SubIds]) ->
  case pe_sub_store:no_gen_server_lookup(SubId) of
    {found, Sub} ->
      OldIds = Sub#pe_sub.tag_ids,
      NewIds = convert_tag_guids_to_keys(OldIds),
      UpdatedSub = Sub#pe_sub{tag_ids=NewIds},
      mnesia:dirty_write(UpdatedSub),
      error_logger:info_msg("Converted tag_ids for Subscription ~p: ~p -> ~p~n",[SubId, OldIds, NewIds]),
      convert_subscription_tag_guids_to_keys(SubIds);
    _NOT_FOUND ->
      {error, {cannot_convert_subscription_tag_guids_to_keys, {sub_not_found, SubId}}}
  end.


convert_tag_guids_to_keys(undefined) ->
  undefined;

convert_tag_guids_to_keys([]) ->
  [];
  
convert_tag_guids_to_keys(TagIds) ->
  lists:map(fun(TagId) ->
    case pe_tag_store:get_by_id(TagId) of
      {found, Tag} ->
        pe_tag_util:create_id(pe_tag_util:get(type, Tag), pe_tag_util:get(value, Tag));
      none ->
        throw({error, {unable_to_convert_tag_guid_to_key, {tag_id_not_found, TagId}}})
      end
  end, TagIds).
