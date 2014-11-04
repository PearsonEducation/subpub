#!/usr/bin/env escript
%% -*- erlang -*-
%%! --smp enable -name upgrade_rec -setcookie COOKIE -mnesia debug verbose -remsh prospero@subpub.pd-cloud.local -pa /opt/prospero/include/

-include("/opt/prospero/include/prospero.hrl").

main([NodeIn|_]) ->
	Node = list_to_atom(NodeIn),
	ok = migrate_record_schema(Node),
	halt().


migrate_record_schema(Node) ->
	MigrationFun = fun({
				pe_principal,
				Id,
				FriendlyName,
				EnforcedTagIds,
				IsRequireMessageTypeWithNewSubs,
				DateCreated,
				DateDeactivated,
				Secret,
				Realm,
				DeliveryUrlMask
			}) ->
				{
                                pe_principal,
                                Id,
                                FriendlyName,
                                EnforcedTagIds,
                                IsRequireMessageTypeWithNewSubs,
                                DateCreated,
                                DateDeactivated,
                                Secret,
                                Realm,
                                DeliveryUrlMask,
                                false
				}
			end,
	{atomic, ok} = rpc:call(Node, mnesia, transform_table, [pe_principal, MigrationFun, record_info(fields, pe_principal)]),
  ok.
