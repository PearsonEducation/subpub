% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_import).
-export([import_principals/1, import_subscriptions/1]).
-include("include/sxml.hrl").
-include("include/prospero.hrl").


import_principals(Filename) ->
	Func = fun(Node) ->
		to_principal(Node)
	end,
	Principals = import_list(Filename, Func),
	lists:foreach(
		fun(Principal) ->
			pe_principal_store:create_new(Principal)
		end,
		Principals
	),
	io:format("imported ~p principals~n", [length(Principals)]).

import_subscriptions(Filename) ->
	Func = fun(Node) ->
		to_subscription(Node)
	end,
	Subscriptions = import_list(Filename, Func),
	lists:foreach(
		fun(Sub) ->
			pe_sub_store:create_new(Sub)
		end,
		Subscriptions
	),
	io:format("imported ~p subscriptions~n", [length(Subscriptions)]).

import_list(Filename, Func) ->
	{XmlRoot, _} = xmerl_scan:file(Filename),
	Root = sxml:to_sxml(xmerl_lib:normalize_element(XmlRoot)),
	node_list_to_list(Root#branch.children, Func).


node_list_to_list([], _) -> [];
node_list_to_list([First | Rest], Func) ->
	[ Func(First) | node_list_to_list(Rest, Func) ].



to_subscription(Node) ->
	{ok, DateCreated} = pe_time:parse_8601(sxml:get_child_value(date_created, Node)),
	{ok, DateCancelled} = pe_time:parse_8601(sxml:get_child_value(date_cancelled, Node)),
	PrincipalId = sxml:get_child_value(principal_id, Node),
	CallbackUrl = sxml:get_child_value(callback_url, Node),
	TagIds = to_tag_id_list(Node),
	DuplicationKey = pe_sub:make_duplication_key(CallbackUrl, "", PrincipalId, TagIds),
	
	#pe_sub{
		id = sxml:get_attr(Node, id),
		principal_id = PrincipalId,
		callback_url = CallbackUrl,
		queue_name = sxml:get_child_value(queue_name, Node),
		date_created = DateCreated,
		date_cancelled = DateCancelled,
		tag_ids = TagIds,
		duplication_key = DuplicationKey
	}.


to_tag_id_list(Node) ->
	ExtractId = fun(TagNode) ->
		sxml:get_attr(TagNode, id)
	end,
	map_tag_node_list(Node, ExtractId).


map_tag_node_list(Node, Func) ->
	case sxml:get_child(tags, Node) of
		undefined -> [];
		Child -> 
	case Child of 
		#leaf{} -> [];
		#branch{children = Children} ->
			node_list_to_tag_list(Children, Func)
	end
	end.


node_list_to_tag_list([], _) -> [];
node_list_to_tag_list([First | Rest], Func) ->
	[ Func(First) | node_list_to_tag_list(Rest, Func) ].


to_principal (Node) ->
	{ok, DateCreated} = pe_time:parse_8601(sxml:get_child_value(date_created, Node)),
	{ok, DateDeactivated} = pe_time:parse_8601(sxml:get_child_value(date_deactivated, Node)),
	RequireMessageType = case sxml:get_child_value(is_require_message_type_with_new_subs, Node) of
		"true" -> true;
		_Otherwise -> false
	end,
	DurableMessagingEnabled = case sxml:get_child_value(durable_messaging_enabled, Node) of
		"true" -> true;
		_Else -> false
	end,
	#pe_principal {
		id = sxml:get_attr(Node, id),
		friendly_name = sxml:get_child_value(friendly_name, Node),
		enforced_tag_ids = to_tag_id_list(Node),
		is_require_message_type_with_new_subs = RequireMessageType,
		date_created = DateCreated,
		date_deactivated = DateDeactivated,
		secret = sxml:get_child_value(secret, Node),
		realm = sxml:get_child_value(realm, Node),
		delivery_url_mask = sxml:get_child_value(delivery_url_mask, Node),
		durable_messaging_enabled = DurableMessagingEnabled
	}.
