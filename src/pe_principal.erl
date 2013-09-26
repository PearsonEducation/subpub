% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_principal).

-export([
	get/2, 
	instance/8, 
	instance/9, 
	is_url_within_mask/2,
	to_string/1
]).

-include("include/prospero.hrl").

get(id, Principal) ->
  Principal#pe_principal.id;
  
get(friendly_name, Principal) ->
  Principal#pe_principal.friendly_name;

get(enforced_tag_ids, Principal) ->
  Principal#pe_principal.enforced_tag_ids;

get(is_require_message_type_with_new_subs, Principal) ->
  Principal#pe_principal.is_require_message_type_with_new_subs;

get(date_created, Principal) ->
  Principal#pe_principal.date_created;

get(date_deactivated, Principal) ->
  Principal#pe_principal.date_deactivated;

get(secret, Principal) ->
  Principal#pe_principal.secret;

get(realm, Principal) ->
  Principal#pe_principal.realm;
  
get(delivery_url_mask, Principal) ->
  Principal#pe_principal.delivery_url_mask.

instance(PrincipalId, FriendlyName, TagRecords, RequireMessageTypeWithSubscription, Secret, DateCreated, Realm, DeliveryUrlMask) ->
  instance(PrincipalId, FriendlyName, TagRecords, RequireMessageTypeWithSubscription, Secret, DateCreated, undefined, Realm, DeliveryUrlMask).

instance(PrincipalId, FriendlyName, TagRecords, RequireMessageTypeWithSubscription, Secret, DateCreated, DateDeactivated, Realm, DeliveryUrlMask) ->
  #pe_principal{
    id=PrincipalId, 
    friendly_name=FriendlyName, 
    enforced_tag_ids=TagRecords, 
    is_require_message_type_with_new_subs=RequireMessageTypeWithSubscription,
    date_created=DateCreated,
    date_deactivated=DateDeactivated,
    secret=Secret,
    realm=Realm,
    delivery_url_mask=DeliveryUrlMask
  }.

%
% convert a pe_principal record/tuple to a string
%
to_string (Principal) ->
	io_lib:format(
		"{ ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p }", 
		[ 
			Principal#pe_principal.id,
			Principal#pe_principal.friendly_name,
			Principal#pe_principal.enforced_tag_ids,
			Principal#pe_principal.is_require_message_type_with_new_subs,
			Principal#pe_principal.date_created,
			Principal#pe_principal.date_deactivated,
			Principal#pe_principal.secret,
			Principal#pe_principal.realm,
			Principal#pe_principal.delivery_url_mask
		]
	).


is_url_within_mask(Url, Principal) when is_record(Principal, pe_principal) ->
  Mask = ?MODULE:get(delivery_url_mask, Principal),
  is_url_within_mask(Url, Mask);
  
is_url_within_mask(_Url, "*") ->
  ok;

is_url_within_mask(Url, "*" ++ Mask) ->
  is_url_within_mask(Url, Mask);

is_url_within_mask(Url, Mask) ->
  {_, NetLoc, _, _, _} = mochiweb_util:urlsplit(Url),
  Regex = lists:flatten(io_lib:format("~s$", [Mask])),
  case re:run(NetLoc, Regex) of
    {match, _Capture} ->
      ok;
    nomatch ->
      not_within
  end.
