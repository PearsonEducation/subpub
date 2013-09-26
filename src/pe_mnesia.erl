% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_mnesia).

-export([
  init_schema_on_new_member/1,
  ensure_started_with_empty_database/0,
  wait_for_all_local_tables/1,
  init_schema/0,
  create_table/1,
  delete_table/1
]).

init_schema() ->
  case mnesia:system_info(use_dir) of
    true ->
      error_logger:info_msg("Schema already exists, attempting to force load from local store...~n"),
      mnesia:start(),
      case force_load_tables() of
        yes ->
          {atomic, ok};
        ERROR ->
          ERROR
      end;
    false ->
      error_logger:info_msg("Creating a new schema from scratch...~n"),
      mnesia:create_schema([node()]),
      mnesia:start(),
      case create_tables(pe_migrate:get_schema()) of
        ok ->
          pe_migrate:set_version(),
          {atomic, ok};
        ERROR ->
          ERROR
      end
  end.

get_existing_tables_except_schema() ->
  lists:filter(fun(E) -> E /= schema end, mnesia:system_info(tables)).
  
force_load_tables() ->
  force_load_tables(get_existing_tables_except_schema()).
  
force_load_tables([]) ->
  yes;
force_load_tables([Table|Tables]) ->
  error_logger:info_msg("Forcing load of table ~p~n",[Table]),
  case mnesia:force_load_table(Table) of
    yes ->
      force_load_tables(Tables);
    ERROR ->
      {unable_to_force_load_table, Table, ERROR}
  end.
  
delete_table(Table) ->
  delete_table(Table, mnesia:table_info(Table, disc_copies)).
  
delete_table(_Table, []) ->
  {atomic, ok};
  
delete_table(Table, [Node | Nodes]) ->
  error_logger:info_msg("Attempting to delete table copy of ~p on ~p~n",[Table, Node]),
  case mnesia:del_table_copy(Table, Node) of
    {atomic, ok} ->
      error_logger:info_msg("Deleted table copy of ~p on ~p~n",[Table, Node]),
      delete_table(Table, Nodes);
    {aborted, Reason} ->
      error_logger:info_msg("Problem deleting table copy of ~p on ~p: ~p~n",[Table, Node, Reason]),
      {aborted, Reason}
  end.
  
create_tables([]) ->
  ok;
create_tables([TableDef|Tables]) ->
  case create_table(TableDef) of
    {atomic, ok} ->
      create_tables(Tables);
    ERROR ->
      ERROR
  end.
  
create_table({Table,{Type, Attributes}}) ->
  mnesia:create_table(Table, [{Type, [node()]} | Attributes]).

init_schema_on_new_member(Node) ->
io:format("init_schema_on_new_member for node ~p", [Node]),
  case mnesia:change_config(extra_db_nodes, [node() | nodes()]) of
    {ok, _ReturnValue} ->
      case ensure_table_as_disc_copy(schema, Node) of
        {atomic, ok}  ->
          case replicate_disc_copies_tables(Node) of
            {atomic, ok} ->
              {atomic, ok};
            ELSE ->
              ELSE
          end;
        {aborted, Reason} ->
          {error, {unable_to_change_schema_table_copy_type, Reason}}
      end;
    {error, Error} ->
      {error, {unable_to_add_extra_db_node, Error}}
  end.


ensure_table_as_disc_copy(Table, Node) ->
  case mnesia:add_table_copy(Table, Node, disc_copies) of
    {atomic, ok} ->
      {atomic, ok};
    {aborted, {already_exists,Table,Node}} ->
      case mnesia:change_table_copy_type(Table, Node, disc_copies) of
        {atomic, ok} ->
          {atomic, ok};
        {aborted, {already_exists,Table,Node,disc_copies}} ->
          {atomic, ok};
        _ELSE ->
          _ELSE
      end;
    _ELSE ->
      _ELSE
  end.


ensure_started_with_empty_database() ->
  mnesia:start(). %TODO: Add logic to backup folder and start fresh.
  
wait_for_all_local_tables(Timeout) ->
  mnesia:wait_for_tables(mnesia:system_info(tables), Timeout).


replicate_disc_copies_tables(Node) ->
  DiscCopyTables = lists:filter(
    fun(Table) ->
      mnesia:table_info(Table, storage_type) == disc_copies andalso Table /= schema 
    end,
    mnesia:system_info(tables)
  ),
  replicate_tables(DiscCopyTables, Node).


replicate_tables([], _Node) ->
  {atomic, ok};
  
replicate_tables([Table|Tables], Node) ->
  case ensure_table_as_disc_copy(Table, Node) of
    {atomic, ok} ->
      replicate_tables(Tables, Node);
    {aborted, Reason} ->
      {error, {unable_to_replicate_table, {Table, Reason}}}
  end.
