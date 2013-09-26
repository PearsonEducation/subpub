% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_principal_event_handler).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_cast/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-include("include/prospero.hrl").

init([]) ->
  {ok, []}.
  
handle_event({principal_deactivated, Principal}, _State) ->
  error_logger:info_msg("Attempting to deactivate all subscriptions for deactivated principal ~p~n",[pe_principal:get(id, Principal)]),
  case pe_sub_store:get_all_for_principal(pe_principal:get(id, Principal)) of
    [] ->
      ok;
    Subs ->
      Fun = fun(Sub) ->
        error_logger:info_msg("Found subscription ~p deactivated principal ~p:  Deleting...~n",[pe_sub:get(id, Sub),pe_principal:get(id, Principal)]),
        pe_sub_intake:delete_async(Sub)
      end,
      lists:foreach(Fun, Subs)
  end,
  {ok,_State};

handle_event(_, _State) ->
  {ok,_State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_call(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
