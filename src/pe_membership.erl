% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_membership).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
 
-export([join/1, form_new/0, start_link/0]).

-include("include/prospero.hrl").

-define(DEFAULT_BOOTSTRAP_JOIN_TIMEOUT_MILLIS, 30000).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

join(BootstrapNode) ->
  gen_server:call(?MODULE, {join, BootstrapNode}, get_timeout() + 5000).

form_new() ->
  gen_server:call(?MODULE, {form_new}).

init([]) ->
  {ok, []}.
  
get_timeout() ->
  pe_config:get(startup, join_timeout_millis, ?DEFAULT_BOOTSTRAP_JOIN_TIMEOUT_MILLIS).
  
wait_for_tables() ->
  Timeout = get_timeout(),
  error_logger:info_msg("Waiting ~p milliseconds for tables...~n",[Timeout]),
  case pe_mnesia:wait_for_all_local_tables(Timeout) of
    ok ->
      error_logger:info_msg("Completed syncing tables~n");
    {timeout, BadTables} ->
      throw({error, {timeout_waiting_for_tables, BadTables}});
    {error, Reason} ->
      throw({error, {error_waiting_for_tables, Reason}})
  end.
  
do_join(BootstrapNode) when is_atom(BootstrapNode) ->
  MYSELF = node(),
  case BootstrapNode of
    MYSELF ->
      error_logger:info_msg("Skipping myself as a bootstrap node ~s~n",[BootstrapNode]),
      myself;
    _ELSE ->
      error_logger:info_msg("Attempting to join bootstrap ~s~n",[BootstrapNode]),
      case net_adm:ping(BootstrapNode) of
        pong ->
          error_logger:info_msg("Successfully pinged bootstrap node ~s.  Known nodes: ~p~n",[BootstrapNode,nodes()]),
          pe_mnesia:ensure_started_with_empty_database(),
          case gen_server:call({?MODULE,BootstrapNode}, {init_schema, node()}) of
            welcome ->
              wait_for_tables(),
              error_logger:info_msg("Successfully initialized schema from bootstrap node ~s~n",[BootstrapNode]),
              ok;
            ERROR ->
              error_logger:error_msg("Could not initialize schema from bootstrap ~s: ~p~n",[BootstrapNode, ERROR]),
              {error, {could_not_init_schema,ERROR}}
          end;
        pang ->
          error_logger:error_msg("Could not ping bootstrap ~s~n",[BootstrapNode]),
          {error, {could_not_ping}}
      end
  end;
  
do_join([]) ->
  {error, no_bootstrap_nodes_available};
  
do_join([Node|BootstrapNodes]) ->
  case do_join(Node) of
    ok ->
      ok;
    myself ->
      do_join(BootstrapNodes);
    {error, {could_not_ping}} ->
      do_join(BootstrapNodes);
    ERROR ->
      ERROR
  end.
  
  
do_form_new() ->
  error_logger:info_msg("Forming a new membership cloud~n"),
  init_schema().
     

handle_call({form_new}, _From, _State) ->
  do_form_new(),
  {reply,ok,_State};

handle_call({join, BootstrapNode}, _From, State) ->
  case do_join(BootstrapNode) of
    ok ->
      {reply, ok, State};
    {error, no_bootstrap_nodes_available} ->
      error_logger:info_msg("None of the supplied bootstrap nodes are available~n"),
      case lists:filter(fun(E) -> E == node() end, BootstrapNode) of
        [_Node] ->
          error_logger:info_msg("I appear to be the first member of this cluster so I will form a new cloud~n"),
          do_form_new(),
          {reply, ok, State};
        _ELSE ->  
          {reply, no_bootstrap_nodes_available, State}
      end;
    ERROR ->
      {reply, ERROR, State}
  end;
  
handle_call({init_schema, Node}, _From, _State) ->
  Result = case init_schema_on_new_member(Node) of
    welcome ->
      welcome;
    {error, Error} ->
      {recommend_halt, {schema_initialization_failed, Error}}
  end,
  {reply, Result, _State}.
  

init_schema_on_new_member(Node) ->
  error_logger:info_msg("Attempting to initialize schema on new member ~p.  Nodes: ~p~n", [Node, nodes()]),
  case pe_mnesia:init_schema_on_new_member(Node) of
    {atomic, ok} ->
      error_logger:info_msg("Initialized schema on new member ~p.  Nodes: ~p~n", [Node, nodes()]),
      welcome;
    {error, Error} ->
      error_logger:info_msg("Initialization failed: ~p~n", [Error]),
      {error, Error}
  end.

                        
init_schema() ->
  case pe_mnesia:init_schema() of
    {atomic, ok} ->
      wait_for_tables(),
      case pe_migrate:migrate() of
        up_to_date ->
          ok;
        ERROR ->
          throw({error, {initialized_existing_schema_but_unable_to_migrate, ERROR}})
      end;
    ERROR ->
      throw({error, {unable_to_init_schema, ERROR}})
  end.
    
%  mnesia:create_table(pe_msg,         [{attributes, record_info(fields, pe_msg)}]),
%  mnesia:create_table(pe_del_order,   [{attributes, record_info(fields, pe_del_order)}]),
%  mnesia:create_table(pe_del_attempt, [{attributes, record_info(fields, pe_del_attempt)},{index, [del_order_id]}]).
  
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.


