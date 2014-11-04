% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
-module(pe_id_broker).

-behaviour(gen_server).

% The pe_id_broker module is reponsible for generating unique identifiers for
% all enitities within SubPub. These entities currently include the following:
%    - SubscriptionId
%    - MessageId
%    - DeliveryAttemptId
%
% APPROACH
%
% This module delegates the UUID generation to an external module, lexical_uuid.erl 
% created by James Golick and available on GitHub here:
%
%    https://github.com/jamesgolick/lexical_uuid.erl
%
% As described in James' README.md, the lexical_uuid generator creates
% "UUIDs that are byte-ordered lamport clocks (timestamp, worker_id)."
% 
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
 
-export([get_next/0, start_link/0]).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_next() ->
  gen_server:call(?MODULE, {next_id}).

init([]) ->
  {ok, []}.
 
handle_call({next_id}, _From, State) ->
  {reply, {id,lexical_uuid:get_string()}, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
