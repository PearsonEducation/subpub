% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_json).

-export([
	decode_principal/1,
	decode_subscription/1,
	encode_principal/2,
	encode_principal_list/2,
	encode_registry/3,
	encode_registry_keys/1,
	encode_registry_contents/1,
	encode_subscription/1,
	encode_subscription_list/1
]).

-include("include/prospero.hrl").

bin(Val) ->
	pe_util:to_binary(Val).

%
% Decode a JSON list of tags
%
decode_tags ([]) -> [];
decode_tags ([Head | Tail]) ->
	Tag = decode_one_tag (Head),
	[Tag | decode_tags(Tail)].

%
% Decode a single JSON tag.
%
% See the description for this file for details on what the JSON is expected
% to look like.
%
decode_one_tag (Struct) ->
	{struct, TagProps} = Struct,
	TagId = binary_to_list(proplists:get_value(<<"id">>, TagProps)),
	TagType = binary_to_list(proplists:get_value(<<"type">>, TagProps)),
	TagValue = binary_to_list(proplists:get_value(<<"value">>, TagProps)),
	#pe_tag {
		id		= TagId,
		type	= TagType,
		value	= TagValue
	}.

decode_principal (JSON) ->
	{struct, PrincipalProps} = mochijson2:decode(JSON),
	Id = binary_to_list(proplists:get_value(<<"id">>, PrincipalProps)),
	FriendlyName = binary_to_list(proplists:get_value(<<"friendly_name">>, PrincipalProps)),
	RequireMessageType = proplists:get_value(<<"require_message_type">>, PrincipalProps),
	Mask = binary_to_list(proplists:get_value(<<"delivery_url_mask">>, PrincipalProps)),
	Realm = binary_to_list(proplists:get_value(<<"realm">>, PrincipalProps)),
	Secret = binary_to_list(proplists:get_value(<<"secret">>, PrincipalProps)),
	Tags = proplists:get_value(<<"enforced_tags">>, PrincipalProps),
	EnforcedTags = decode_tags(Tags),
	DurableMessagingEnabled = proplists:get_value(<<"durable_messaging_enabled">>, PrincipalProps, false),
	#pe_principal{
		id 					= Id, 
		friendly_name 		= FriendlyName, 
		is_require_message_type_with_new_subs = RequireMessageType,
		realm 				= Realm,
		delivery_url_mask 	= Mask,
		durable_messaging_enabled = DurableMessagingEnabled,
		secret 				= Secret,
		enforced_tag_ids	= EnforcedTags
	}.

encode_tags(Ids) ->
	encode_tags(pe_tag_util:instances(Ids), []).

encode_tags([Tag|Tail], Result) ->
	encode_tags(Tail, [encode_one_tag(Tag)|Result]);

encode_tags([], Result) ->
	Result.

encode_one_tag(Tag) ->
	{struct, [
		{id,bin(Tag#pe_tag.id)},
		{type,bin(Tag#pe_tag.type)},
		{value,bin(Tag#pe_tag.value)}
	]}.

encode_principal(Principal, IncludeSecret) ->
	{struct, [
		{ id, bin(Principal#pe_principal.id) },
		{ friendly_name, bin(Principal#pe_principal.friendly_name) },
		{ require_message_type, Principal#pe_principal.is_require_message_type_with_new_subs },
		{ enforced_tag_ids, encode_tags(Principal#pe_principal.enforced_tag_ids) },
		{ date_created, bin(pe_time:format_8601(Principal#pe_principal.date_created)) },
		{ date_deactivated, bin(pe_time:format_8601(Principal#pe_principal.date_deactivated)) },
		{ realm, bin(Principal#pe_principal.realm) },
		{ delivery_url_mask, bin(Principal#pe_principal.delivery_url_mask) },
		{ 
			secret, 
			case IncludeSecret of
				true -> bin(Principal#pe_principal.secret);
				_ -> bin("")
			end
		},
		{ durable_messaging_enabled, Principal#pe_principal.durable_messaging_enabled }
	]}.

encode_principal_list(Principals, IncludeSecret) ->
	lists:map (
		fun(Principal) ->
			encode_principal(Principal, IncludeSecret)
		end,
		Principals
	).

sub_decode_one_tag(Struct) ->
	{ struct, Properties } = Struct,
	binary_to_list(proplists:get_value(<<"id">>, Properties)).


sub_decode_tags([]) -> [];
sub_decode_tags([Struct | Tail]) ->
	[ sub_decode_one_tag(Struct) | sub_decode_tags(Tail) ].

decode_subscription(JSON) ->
	Result = mochijson2:decode(JSON),
	{struct, SubscriptionProperties} = Result,
	PrincipalId = binary_to_list(proplists:get_value(<<"principal_id">>, SubscriptionProperties)),
	Callback = binary_to_list(proplists:get_value(<<"callback_url">>, SubscriptionProperties)),
	TagStructs = proplists:get_value(<<"tags">>, SubscriptionProperties),
	Tags = sub_decode_tags(TagStructs),
	#pe_sub {
		principal_id = PrincipalId,
		callback_url = Callback,
		tag_ids = Tags
	}.

encode_subscription(Subscription) ->
	{struct, [
		{id,bin(pe_sub:get(id,Subscription))},
		{principal_id,bin(pe_sub:get(principal_id,Subscription))},
		{callback_url,bin(pe_sub:get(callback_url,Subscription))},
		{wsdl_uri,bin(pe_sub:get(wsdl_uri,Subscription))},
		{queue_name,bin(pe_sub:get(queue_name,Subscription))},
		{date_created,bin(pe_time:format_8601(pe_sub:get(date_created, Subscription)))},
		{date_cancelled,bin(pe_time:format_8601(pe_sub:get(date_cancelled, Subscription)))},
		{tags, encode_tags(pe_sub:get(tag_ids,Subscription))}
	]}.

encode_subscription_list(Subscriptions) ->
	lists:map(
		fun(Subscription) ->
			encode_subscription(Subscription)
		end,
		Subscriptions
	).

encode_registry(Dictionary, StartTime, UpTime) ->
	{struct, [
		{startTime, StartTime},
		{upTime, UpTime},
		{messages, encode_registry_entries(Dictionary)}
	]}.

encode_registry_entries(Dictionary) ->
	lists:map(
		fun(Key) ->
			Value = dict:fetch(Key, Dictionary),
			encode_registry_message(Value)
		end,
		dict:fetch_keys(Dictionary)
	).

encode_registry_message (Message) ->
	{struct, [
		{key, encode_message_key(Message#registry_message.key)},
		{totalBytes, Message#registry_message.totalBytes},
		{totalMessages, Message#registry_message.totalMessages},
		{content, bin(Message#registry_message.content)}
	]}.

encode_message_key(MessageKey) ->
	{struct, [
		{client, bin(MessageKey#message_key.client)},
		{clientString, bin(MessageKey#message_key.clientString)},
		{messageType, bin(MessageKey#message_key.messageType)},
		{system, bin(MessageKey#message_key.system)},
		{subSystem, bin(MessageKey#message_key.subSystem)}
	]}.

encode_registry_key(Key) ->
	{struct, [
		{key, bin(Key)}
	]}.

encode_registry_keys(Keys) ->
	lists:map(
		fun(Key) ->
			encode_registry_key(Key)
		end,
		Keys
	).

encode_registry_entry(Tuple) ->
	[ Key, Value ] = tuple_to_list(Tuple),
	{struct, [
		{key, bin(Key)},
		{value, bin(Value)}
	]}.
		

encode_registry_contents(Contents) ->
	lists:map(
		fun(Entry) ->
			encode_registry_entry(Entry)
		end,
		Contents
	).
