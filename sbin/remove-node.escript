#!/usr/bin/env escript
%% -*- erlang -*-
%%! --smp enable -name remove_nodes -setcookie COOKIE -mnesia debug verbose -remsh prospero@subpub01.pd-cloud.local

main([ThisNode|Nodes]) ->
	Me = list_to_atom(ThisNode),
        ok = destroy_schema(Me),
        ok = remove_node_from_cluster(Me, Nodes),
        halt().

destroy_schema(Me) ->
	rpc:call(Me, mnesia, stop, []),
	rpc:call(Me, mnesia, delete_schema, [Me]),
	ok.

remove_node_from_cluster(_, []) ->
        ok;
remove_node_from_cluster(Me, [Node|Nodes]) ->
        RemNode = list_to_atom(Node),
        rpc:call(RemNode, mnesia, del_table_copy, [schema, Me]),
        remove_node_from_cluster(Me, Nodes).
