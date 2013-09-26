% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_sub_event_handler).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_cast/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-include("include/prospero.hrl").

init([]) ->
  {ok, []}.
  
handle_event({sub_created, Subscription}, _State) ->
  amqp_manager:watch_subscription(Subscription),
  {ok,_State};

handle_event({sub_cancelled, SubId}, _State) ->
  amqp_manager:unwatch_subscription(SubId, true),
  {ok,_State};

handle_event(_, _State) ->
  {ok,_State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_call(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
