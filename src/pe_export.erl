% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_export).

-include("include/prospero.hrl").

-export([export_principals/1, export_subscriptions/1]).

-define(EXPORT_VERSION,"2").

make_sub_data({found,Subscription}, TagTemplate) ->
	[
		{id, pe_sub:get(id, Subscription)},
		{principal_id, pe_sub:get(principal_id, Subscription)},
		{callback_url, pe_sub:get(callback_url, Subscription)},
		{queue_name, pe_sub:get(queue_name, Subscription)},
		{date_created, pe_time:format_8601(pe_sub:get(date_created, Subscription))},
		{date_cancelled, pe_time:format_8601(pe_sub:get(date_cancelled, Subscription))},
		{tags, get_tags(pe_sub:get(tag_ids, Subscription))},
		{tag_template, TagTemplate}
	].

get_tags(undefined) -> [];
get_tags(Ids) ->
	get_tag_data(pe_tag_util:instances(lists:sort(Ids))).

get_tag_data(Tags) ->
	get_tag_data(Tags, []).

get_tag_data([Tag|Tags], Results) ->
	Tmp = [
		{id, pe_tag_util:get(id, Tag)},
		{type, pe_tag_util:get(type, Tag)},
		{value, pe_tag_util:get(value, Tag)}
	],
	get_tag_data(Tags, [Tmp|Results]);

get_tag_data([], Results) ->
	Results.
  
get_all_subscriptions(TagTemplate) ->
	get_all_subscriptions(
		lists:sort(pe_sub_store:get_all_ids()),[], TagTemplate).

get_all_subscriptions([Id|Ids],Results, TagTemplate) ->
get_all_subscriptions(Ids,[make_sub_data(pe_sub_store:lookup(Id), TagTemplate)|Results], TagTemplate);

get_all_subscriptions([],Results, _TagTemplate) ->
	Results.
	  
get_all_principals(TagTemplate) ->
	get_all_principals(
		lists:sort(pe_principal_store:get_all_ids()),[], TagTemplate).

get_all_principals([Id|Ids],Results, TagTemplate) ->
	get_all_principals(Ids,[make_principal_data(pe_principal_store:lookup(Id), TagTemplate)|Results], TagTemplate);
  
get_all_principals([],Results, _TagTemplate) ->
	Results.
 

make_principal_data({found,Principal}, TagTemplate) ->
	[
		{id, pe_principal:get(id, Principal)},
		{friendly_name, pe_principal:get(friendly_name, Principal)},
		{is_require_message_type_with_new_subs, pe_principal:get(is_require_message_type_with_new_subs, Principal)},
		{date_created, pe_time:format_8601(pe_principal:get(date_created, Principal))},
		{date_deactivated, pe_time:format_8601(pe_principal:get(date_deactivated, Principal))},
		{secret, pe_principal:get(secret, Principal)},
		{realm, pe_principal:get(realm, Principal)},
		{delivery_url_mask, pe_principal:get(delivery_url_mask, Principal)},
		{durable_messaging_enabled, pe_principal:get(durable_messaging_enabled, Principal)},
		{tags, get_tags(pe_principal:get(enforced_tag_ids, Principal))},
		{tag_template, TagTemplate}
	].  

export_principals(File) ->
	{ok, TagTemplate} = sgte:compile_file("priv/tag.sgte"),
	{ok, PrincipalTemplate} = sgte:compile_file("priv/principal.sgte"),
	{ok, PrincipalsTemplate} = sgte:compile_file("priv/principals_export.sgte"),

	Now = pe_time:format_8601(),

	Result = sgte:render_str(
		PrincipalsTemplate, [
			{principal_template, PrincipalTemplate},
			{principals, get_all_principals(TagTemplate)},
			{exported, Now},
			{version, ?EXPORT_VERSION}
		]
	),

	{ok, Handle} = file:open(File, [write]),
	io:format(Handle, Result, []),
	file:close(Handle).

export_subscriptions(File) ->
	{ok, TagTemplate} = sgte:compile_file("priv/tag.sgte"),
	{ok, SubTemplate} = sgte:compile_file("priv/subscription.sgte"),
	{ok, SubsTemplate} = sgte:compile_file("priv/subscriptions_export.sgte"),

	Now = pe_time:format_8601(),

	Result = sgte:render_str(
		SubsTemplate, [
			{sub_template,SubTemplate},
			{subs,get_all_subscriptions(TagTemplate)},
			{exported,Now},
			{version,?EXPORT_VERSION}
		]
	),

	{ok, Handle} = file:open(File, [write]),
	io:format(Handle, Result, []),
	file:close(Handle).


