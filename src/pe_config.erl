% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_config).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
 
-export([get/3, set/3, start_link/0]).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get(Section, Key, Default) ->
  case ets:lookup(?MODULE, {Section, Key}) of
    [] -> Default;
    [{_, undefined}] -> Default;
    [{_, Match}] -> Match
  end.
  
set(Section, Key, Value) ->
  gen_server:call(?MODULE, {set, Section, Key, Value}).

init([]) ->
  ets:new(?MODULE, [named_table, set, protected]),
  {ok, []}.
 
handle_call({set, Section, Key, Value}, _From, State) ->
  OldValue = ?MODULE:get(Section, Key, undefined),
  true = ets:insert(?MODULE, {{Section, Key}, Value}),
  
  error_logger:info_msg("CONFIG CHANGE: ~p~n",[[{section,Section},{key,Key},{new_value,Value},{old_value,OldValue}]]),
  
  {reply, ok, State}.
 
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
