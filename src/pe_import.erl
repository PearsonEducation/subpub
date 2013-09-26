% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_import).
-compile(export_all).
-include("include/sxml.hrl").
-include("include/prospero.hrl").


all_prin() ->
	Fun = fun(ID) ->
		io:format("~p~n", [pe_principal_store:lookup(ID)])
	end,
	lists:foreach(Fun, mnesia:dirty_all_keys(pe_principal)).


import(Directory) ->
	SubsFilePath = Directory ++ "/subscriptions.xml",
	Subscriptions = import_subscriptions(SubsFilePath),
	lists:foreach(fun(Sub) -> pe_sub_store:create_new(Sub) end, Subscriptions),

	PrincipalsFilePath = Directory ++ "/principals.xml",
	Principals = import_principals(PrincipalsFilePath),
	lists:foreach(fun(Sub) -> pe_principal_store:create_new(Sub) end, Principals).


import_principals(Filename) ->
	Func = fun(Node) ->
		to_principal(Node)
	end,
	import_list(Filename, Func).

import_subscriptions(Filename) ->
	Func = fun(Node) ->
		to_subscription(Node)
	end,
	import_list(Filename, Func).

import_list(Filename, Func) ->
	{XmlRoot, _} = xmerl_scan:file(Filename),
	Root = sxml:to_sxml(xmerl_lib:normalize_element(XmlRoot)),
	node_list_to_list(Root#branch.children, Func).


node_list_to_list([], _) -> [];
node_list_to_list([First | Rest], Func) ->
	[ Func(First) | node_list_to_list(Rest, Func) ].



% to_record(Branch) when is_record(Branch, branch) ->
%	List = add_attrs([to_atom(Branch#branch.name)], Branch#branch.attrs),
%	ChildList = node_list_to_list(Branch#branch.children),
%	list_to_tuple(lists:append(List, ChildList));
%
%to_record(Leaf) when is_record(Leaf, leaf) ->
%	List = add_attrs([to_atom(Leaf#leaf.name)], Leaf#leaf.attrs),
%	list_to_tuple(List);
%	
%to_record([_First | _Rest]) -> unknown.
	

to_atom(Atom) when is_atom(Atom) -> Atom;
to_atom(Atom) when is_list(Atom) -> list_to_atom(Atom).


create_record(Name, AttrList) ->
	add_attrs([to_atom(Name)], AttrList).


add_attrs(OutList, []) -> OutList;
add_attrs(OutList, [{_Name, Value}| Rest]) ->
	List = lists:append(OutList, [Value]),
	add_attrs(List, Rest).


to_tag(Node) ->
	#pe_tag{
		id = sxml:get_attr(Node, id),
		type = (sxml:get_child(type, Node))#leaf.value,
		value = (sxml:get_child(value, Node))#leaf.value
	}.

to_subscription(Node) ->
	{ok, DateCreated} = pe_time:parse_8601(sxml:get_child_value(date_created, Node)),
	{ok, DateCancelled} = pe_time:parse_8601(sxml:get_child_value(date_cancelled, Node)),

	#pe_sub{
		id = sxml:get_attr(Node, id),
		principal_id = sxml:get_child_value(principal_id, Node),
		callback_url = sxml:get_child_value(callback_url, Node),
		queue_name = sxml:get_child_value(queue_name, Node),
		date_created = DateCreated,
		date_cancelled = DateCancelled,
		tag_ids = to_tag_id_list(Node)
	}.


to_tag_id_list(Node) ->
	ExtractId = fun(TagNode) ->
		sxml:get_attr(TagNode, id)
	end,
	map_tag_node_list(Node, ExtractId).


to_tag_list(Node) ->
	ExtractTag = fun(TagNode) ->
		to_tag(TagNode)
	end,
	map_tag_node_list(Node, ExtractTag).


map_tag_node_list(Node, Func) ->
	case sxml:get_child(tags, Node) of
		undefined -> undefined;
		Child -> 
	case Child of 
		#leaf{} -> undefined;
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

	#pe_principal {
		id = sxml:get_attr(Node, id),
		friendly_name = sxml:get_child_value(friendly_name, Node),
		enforced_tag_ids = to_tag_id_list(Node),
		is_require_message_type_with_new_subs = 
			sxml:get_child_value(is_require_message_type_with_new_subs, Node),
		date_created = DateCreated,
		date_deactivated = DateDeactivated,
		secret = sxml:get_child_value(secret, Node),
		realm = sxml:get_child_value(realm, Node)
	}.
