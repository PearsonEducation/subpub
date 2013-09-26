% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_msg_event_handler).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_cast/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-include("include/prospero.hrl").

init([]) ->
  {ok, []}.
  
handle_event({msg_posted, _MsgId}, _State) ->
  pe_status:record(msg_posted),
  {ok,_State};

handle_event({msg_delivered, _MsgId, _SubId}, _State) ->
  pe_status:record(msg_delivered),
  {ok,_State};

handle_event({delivery_failed, _DeliveryAttemptId}, _State) ->
  pe_status:record(delivery_failed),
  {ok,_State};

handle_event(_, _State) ->
  {ok,_State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_call(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
