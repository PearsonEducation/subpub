% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.


-module(pe_migrate).

-export([migrate/0, get_schema/0, set_version/0, get_db_version/0]).

-include("include/prospero.hrl").

get_db_version() ->
  case lists:any(fun(Elem) -> Elem == pe_properties end, mnesia:system_info(tables)) of
    false ->
      r1;
    true ->
      case mnesia:dirty_read({pe_properties,version}) of
        [#pe_kvpair{value=Version}] ->
          Version;
        _ELSE ->
          unknown
      end
  end.
  
migrate() ->
  migrate(get_db_version()).

migrate(unknown) ->
  schema_version_unknown;
  
migrate(r1) ->
  error_logger:info_msg("Schema version r1 detected...upgrading to version r2~n"),
  
  case pe_migrate_functions:convert_principal_enforced_tag_guids_to_keys() of
    ok ->
      case pe_migrate_functions:convert_subscription_tag_guids_to_keys() of
        ok ->
          %case pe_mnesia:delete_table(pe_tag) of
            %{atomic, ok} ->
              [PropertiesTableDef|_ELSE] = get_schema(r2),
              pe_mnesia:create_table(PropertiesTableDef),
              set_version(r2),
          
              error_logger:info_msg("Migrated schema from r1 to r2~n"),
              migrate();
            %ERROR ->
              %{error, {unable_to_delete_obsolete_table, pe_tag, ERROR}}
          %end;
        ERROR ->
          ERROR
      end;
    ERROR ->
      ERROR
  end;
  
migrate(r2) ->
  error_logger:info_msg("Schema version r2 detected...upgrading to version r2_1~n"),
  %There are no changes for the r2_1 release, so just bump the version.
  set_version(r2_1),
  migrate();
  
migrate(r2_1) ->
  error_logger:info_msg("Schema version r2_1 detected...upgrading to version r3~n"),
  %There are no changes for the r3 release, so just bump the version.
  set_version(r3),
  migrate();

migrate(r3) ->
  error_logger:info_msg("Schema is up to date at version r3~n"),
  up_to_date;

migrate(r3_1) ->
  error_logger:info_msg("Schema is up to date at version r3~n"),
  up_to_date.


get_schema() ->
  get_schema(?PROSPERO_VERSION).
  
get_schema(r1) ->
  [
    {pe_sub,       {disc_copies, [{attributes, record_info(fields, pe_sub)}]}},
    {pe_tag,       {disc_copies, [{attributes, record_info(fields, pe_tag)}]}},
    {pe_principal, {disc_copies, [{attributes, record_info(fields, pe_principal)}]}}
  ];
  
get_schema(r2) ->
  [
    {pe_properties, {disc_copies, [{attributes, record_info(fields, pe_kvpair)}, {record_name, pe_kvpair}]}},
    {pe_sub,        {disc_copies, [{attributes, record_info(fields, pe_sub)}]}},
    {pe_principal,  {disc_copies, [{attributes, record_info(fields, pe_principal)}]}}
  ];

get_schema(r2_1) ->
  get_schema(r2);

get_schema(r3) ->
  get_schema(r2_1);

get_schema(r3_1) ->
  get_schema(r2_1).


set_version() ->
  set_version(?PROSPERO_VERSION).
  
set_version(Version) ->
  mnesia:dirty_write(pe_properties, #pe_kvpair{key=version, value=Version}). 

