% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_remote).

-export([
  activate_principal/2,
  broker_status/1,
  check_garbage_collect/1,
  consume_from_broker/2,
  create_principal/9,
  create_subscription/5,
  deactivate_principal/2,
  delete_subscription/2,
  export_principals/2,
  export_subscriptions/2,
  find_subscribers/2,
  garbage_collect/1,
  import_principals/2,
  import_subscriptions/2,
  migrate/1,
  print_principal/2,
  print_subscription/2,
  print_all_subscriptions/1,
  print_all_principals/1,
  publish_to_broker/2,
  register_broker/9,
  reset_all_sub_monitors/1,
  reset_logs/2,
  rest_server_in/1,
  rest_server_out/1,
  schema_version/1,
  start_subscription/2,
  stop/1,
  stop_all_sub_monitors/1,
  stop_consuming_from_broker/2,
  stop_publishing_to_broker/2,
  stop_subscription/2,
  upgrade_principal_schema/1
]).
  
reset_logs(Node, SaslLogFile) ->
  rpc:call(Node,error_logger,logfile,[close]),
  ok = rpc:call(Node,error_logger,logfile,[{open, SaslLogFile}]),
  ok = rpc:call(Node,pe_audit,reset,[]),
  
  io:format('done.~n'),
  halt().

stop(Node) ->
  ok = rpc:call(Node,pe_app,stop_and_halt,[]),
  io:format('done.~n'),
  halt().
  
create_subscription(Node, CallbackUrl, TagsString, PrincipalId, WsdlUri) ->
  ReturnVal = case pe_tag_util:parse_tags_string(TagsString) of
    {ok, Tags} ->
      case rpc:call(Node,pe_sub_intake,accept,[CallbackUrl, Tags, PrincipalId, WsdlUri]) of
        {ok,Subscription} ->
          io:format("Created Subscription:~n~p~n",[Subscription]),
          0;
        {error, Error} ->
          io:format("Error while attempting to create subscription: ~p~n",[Error]),
          1;
        {badrpc,nodedown} ->
          io:format("Unable to connect to node while attempting to create subscription: ~p~n",[Node]),
          1;
        Error ->
          io:format("Unknown error while attempting to create subscription: ~p~n",[Error]),
          1
      end;
    Error ->
      io:format("Cannot create principal: Unparseable tag string: ~p~n",[Error]),
      1
  end,
  io:format('done.~n'),
  halt(ReturnVal).
  

delete_subscription(Node, Id) ->
  ReturnVal = case rpc:call(Node,pe_sub_intake,delete,[Id]) of
    {ok,Sub} ->
      io:format("Deleted subscription:~n~p~n",[Sub]),
      0;
    Error ->
      io:format("Unknown error while attempting to delete subscription: ~p~n",[Error]),
      1
  end,
  io:format('done.~n'),
  halt(ReturnVal).

stop_subscription(Node, Id) ->
  ReturnVal = case rpc:call(Node, amqp_manager, unwatch_subscription, [Id, false]) of
    ok ->
      io:format("Stopping all subscription watchers for subscription ~s~n",[Id]),
      0;
    ERROR ->
      io:format("Error stopping all subscription watchers for subscription: ~p~n",[ERROR]),
      1
  end,
  io:format('done.~n'),
  halt(ReturnVal).

start_subscription(Node, Id) ->
  ReturnVal = case rpc:call(Node, amqp_manager, watch_subscription, [{id, Id}]) of
    ok ->
      io:format("Starting subscription watchers for subscription ~s~n",[Id]),
      0;
    ERROR ->
      io:format("Error starting subscription watchers for subscription: ~p~n",[ERROR]),
      1
  end,
  io:format('done.~n'),
  halt(ReturnVal).

create_principal(Node, Id, FriendlyName, TagsString, IsMessageTypeRequiredForSubscription, Secret, Realm, DeliveryUrlMask, DurableMessagingEnabled) ->
  ReturnVal = case pe_tag_util:parse_tags_string(TagsString) of
    {ok, Tags} ->
      case rpc:call(Node,pe_principal_intake,accept,[Id, FriendlyName, Tags, IsMessageTypeRequiredForSubscription, Secret, Realm, DeliveryUrlMask, DurableMessagingEnabled]) of
        {ok,Principal} ->
          io:format("Created Principal:~n~p~n",[Principal]),
          0;
        {error, Error} ->
          io:format("Error while attempting to create principal: ~p~n",[Error]),
          1;
        {badrpc,nodedown} ->
          io:format("Unable to connect to node while attempting to create principal: ~p~n",[Node]),
          1;
        Error ->
          io:format("Unknown error while attempting to create principal: ~p~n",[Error]),
          1
      end;
    Error ->
      io:format("Cannot create principal: Unparseable tag string: ~p~n",[Error]),
      1
  end,
  io:format('done.~n'),
  halt(ReturnVal).
  
deactivate_principal(Node, Id) ->
  ReturnVal = case rpc:call(Node,pe_principal_intake,deactivate,[{id, Id}]) of
    {ok,Principal} ->
      io:format("Deactivated Principal:~n~p~n",[Principal]),
      0;
    Error ->
      io:format("Unknown error while attempting to deactivate principal: ~p~n",[Error]),
      1
  end,
  io:format('done.~n'),
  halt(ReturnVal).

activate_principal(Node, Id) ->
  ReturnVal = case rpc:call(Node,pe_principal_intake,activate,[{id, Id}]) of
    {ok,Principal} ->
      io:format("Activated Principal:~n~p~n",[Principal]),
      0;
    Error ->
      io:format("Unknown error while attempting to activate principal: ~p~n",[Error]),
      1
  end,
  io:format('done.~n'),
  halt(ReturnVal).

stop_all_sub_monitors(Node) ->
  ReturnVal = case rpc:call(Node,amqp_manager,unwatch_all,[false, 30000], 35000) of
    ok ->
      io:format("Stopped all subscription monitors~n"),
      0;
    Error ->
      io:format("Unknown error while attempting to stop all subscription monitors: ~p~n",[Error]),
      1
  end,
  io:format('done.~n'),
  halt(ReturnVal).

reset_all_sub_monitors(Node) ->
  ReturnVal = case rpc:call(Node,amqp_manager,unwatch_all,[false, 30000], 35000) of
    ok ->
      io:format("Stopped all subscription monitors~n"),
      case rpc:call(Node,amqp_manager,watch_all,[],30000) of
        ok ->
          io:format("Started all subscription monitors~n"),
          0;
        Error ->
          io:format("Unknown error while attempting to start all subscription monitors: ~p~n",[Error]),
          1
      end;
    Error ->
      io:format("Unknown error while attempting to stop all subscription monitors: ~p~n",[Error]),
      1
  end,
  io:format('done.~n'),
  halt(ReturnVal).

rest_server_in(Node) ->
  ReturnVal = case rpc:call(Node,pe_rest,server_in,[]) of
    ok ->
      io:format("Server in~n"),
      0;
    Error ->
      io:format("Unknown error while attempting to set server in: ~p~n",[Error]),
      1
  end,
  io:format('done.~n'),
  halt(ReturnVal).

rest_server_out(Node) ->
  ReturnVal = case rpc:call(Node,pe_rest,server_out,[]) of
    ok ->
      io:format("Server out~n"),
      0;
    Error ->
      io:format("Unknown error while attempting to set server out: ~p~n",[Error]),
      1
  end,
  io:format('done.~n'),
  halt(ReturnVal).

register_broker(Node, Key, Host, Port, VirtualHost, Exchange, Username, Password, HeartbeatMillis) ->
  Params = amqp_manager:make_broker_params(Key, [
    {exchange, Exchange},
    {host, Host},
    {port, Port},
    {virtual_host, VirtualHost},
    {username, Username},
    {password, Password},
    {heartbeat_milliseconds, HeartbeatMillis}
  ]),
  ReturnVal = case rpc:call(Node, amqp_manager, register_broker, [Key, Params]) of
    ok ->
      io:format("Registered Broker~n"),
      0;
    Error ->
      io:format("Unknown error while attempting to register broker: ~p~n",[Error]),
      1
  end,
  io:format('done.~n'),
  halt(ReturnVal).     
  
publish_to_broker(Node, Key) ->
  ReturnVal = case rpc:call(Node, amqp_manager, publish_to_broker, [Key]) of
    ok ->
      io:format("Now publishing to broker ~p~n",[Key]),
      0;
    Error ->
      io:format("Unknown error while attempting to enable publishing to broker ~p: ~p~n",[Key, Error]),
      1
  end,
  io:format('done.~n'),
  halt(ReturnVal).

stop_publishing_to_broker(Node, Key) ->
  ReturnVal = case rpc:call(Node, amqp_manager, stop_publishing_to_broker, [Key]) of
    ok ->
      io:format("Stopped publishing to broker ~p~n",[Key]),
      0;
    Error ->
      io:format("Unknown error while attempting to disable publishing to broker ~p: ~p~n",[Key, Error]),
      1
  end,
  io:format('done.~n'),
  halt(ReturnVal).

consume_from_broker(Node, Key) ->
  ReturnVal = case rpc:call(Node, amqp_manager, consume_from_broker, [Key]) of
    ok ->
      io:format("Now consuming from broker ~p~n",[Key]),
      0;
    Error ->
      io:format("Unknown error while attempting to enable consuming from broker ~p: ~p~n",[Key, Error]),
      1
  end,
  io:format('done.~n'),
  halt(ReturnVal).

stop_consuming_from_broker(Node, Key) ->
  ReturnVal = case rpc:call(Node, amqp_manager, stop_consumers_at_broker, [Key]) of
    ok ->
      io:format("Stopped consuming from broker ~p~n",[Key]),
      0;
    Error ->
      io:format("Unknown error while attempting to stop consuming from broker ~p: ~p~n",[Key, Error]),
      1
  end,
  io:format('done.~n'),
  halt(ReturnVal).

broker_status(Node) ->
  ReturnVal = case rpc:call(Node, amqp_manager, broker_status, []) of
    {ok, Status} ->
      io:format("~p~n",[Status]),
      0;
    Error ->
      io:format("Unknown error while attempting to get broker status: ~p~n",[Error]),
      1
  end,
  io:format('done.~n'),
  halt(ReturnVal).

print_subscription(Node, SubId) ->
  ReturnVal = case rpc:call(Node, pe_sub_store, lookup_string, [SubId]) of
    Output ->
      io:format("~s~n",[binary_to_list(Output)]),
      0
  end,
  io:format('done.~n'),
  halt(ReturnVal).

print_principal(Node, Id) ->
  ReturnVal = case rpc:call(Node, pe_principal_store, lookup_string, [Id]) of
    Output ->
      io:format("~s~n",[binary_to_list(Output)]),
      0
  end,
  io:format('done.~n'),
  halt(ReturnVal).

migrate(Node) ->
  ReturnVal = case rpc:call(Node, pe_migrate, migrate, []) of
    up_to_date ->
      io:format("Schema version is up to date.~n"),
      0;
    ERROR ->
      io:format("Encountered a problem while migrating schem: ~p~n",[ERROR]),
      1
  end,
  io:format('done.~n'),
  halt(ReturnVal).

schema_version(Node) ->
  ReturnVal = case rpc:call(Node, pe_migrate, get_db_version, []) of
    Output ->
      io:format("Schema is at version ~p~n",[Output]),
      0
  end,
  io:format('done.~n'),
  halt(ReturnVal).

find_subscribers(Node, MessageType) ->
	ReturnVal = case rpc:call(Node, pe_sub_store, find_by_message_type, [MessageType]) of
		Output ->
			print_subscribers(Output),
			0
	end,
	io:format('done.~n'),
	halt(ReturnVal).

%
% print out a list of subscriptions
%
print_subscribers([]) -> ok;
print_subscribers([Subscriber | Tail]) ->
	io:format("~s~n", [pe_sub:to_string(Subscriber)]),
	print_subscribers(Tail).

%
% print out all the subscriptions on a particular node.
%
% This function makes an RPC call to get all the subscriptions in the database
% and then prints them out.
%
print_all_subscriptions(Node) ->
	ReturnVal = case rpc:call(Node, pe_sub_store, get_all_subscriptions, []) of
		Output ->
			print_subscribers(Output),
			0
	end,
	io:format('done.~n'),
	halt(ReturnVal).

%
% print out a list of principals
%
print_principals([]) -> ok;
print_principals([Principal | Tail]) ->
	io:format("~s~n", [pe_principal:to_string(Principal)]),
	print_principals(Tail).

%
% print out all principals
%
% This function makes an RPC call to get a list of principals from a remote
% node and then prints out that list.
%
print_all_principals(Node) ->
	ReturnVal = case rpc:call(Node, pe_principal_store, get_all_principals, []) of
		Output ->
			print_principals(Output),
			0
	end,
	io:format('done.~n'),
	halt(ReturnVal).

%
% run garbage collection
%
garbage_collect(Node) ->
	ok = rpc:call(Node, pe_collect, collect, []),
	io:format("done.~n", []),
	halt().

check_garbage_collect(Node) ->
	ok = rpc:call(Node, pe_collect, check, []),
	io:format("done.~n", []),
	halt().

export_principals(Node, File) ->
	ok = rpc:call(Node, pe_export, export_principals, [File]),
	io:format("done.~n", []),
	halt().

export_subscriptions(Node, File) ->
	ok = rpc:call(Node, pe_export, export_subscriptions, [File]),
	io:format("done.~n", []),
	halt().

import_principals(Node, File) ->
	ok = rpc:call(Node, pe_import, import_principals, [File]),
	io:format("done.~n", []),
	halt().

import_subscriptions(Node, File) ->
	ok = rpc:call(Node, pe_import, import_subscriptions, [File]),
	io:format("done.~n", []),
	halt().

upgrade_principal_schema(Node) ->
	ok = rpc:call(Node, pe_migrate, migrate_record_schema, []),
	io:format("done.~n", []),
	halt().
