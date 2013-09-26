% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_stats_event_handler).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_cast/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-include("include/prospero.hrl").

init([]) ->
  {ok, []}.

%% Stats event handlers

% Event handler for incrementing statsd counters that record the sending of a message that has been received from RabbitMQ.
% Counters are created for all of SubPub (the entire cluster), per node and per subscription.
handle_event({msg_posted, _MsgId}, _State) ->
  statsd:increment([list_to_binary(string:join(["SubPub", "Nodes", get_node(), "msg_posted"], ".")),
	  list_to_binary("SubPub.msg_posted")]),
  {ok,_State};

% Event handler for incrementing statsd counters that record the sending of a message that has been received from RabbitMQ.
% Counters are created for all of SubPub (the entire cluster), per node and per subscription.
handle_event({send_response, Status}, _State) ->
  statsd:increment([list_to_binary(string:join(["SubPub", "ResponseCodes", integer_to_list(Status)], ".")),
	  list_to_binary(string:join(["SubPub", "Nodes", get_node(), "ResponseCodes", integer_to_list(Status)], "."))]),
  {ok,_State};

% Event handler for incrementing statsd counters that record the arrival of a message to SubPub
% regardless of type or if the message is valid or not.
% Counters are created for all of SubPub (the entire cluster), per node and per subscription.
handle_event({handle_request, Resource}, _State) ->
  statsd:increment([list_to_binary("SubPub.handle_request"),
    list_to_binary(string:join(["SubPub", "Resources", Resource], ".")),
  	list_to_binary(string:join(["SubPub", "Nodes", get_node(), "Resources", Resource], "."))]),
  {ok,_State};

% Event handler for incrementing statsd counters messages successfully delivered (200 or 201 response)
% to the subscribers endpoint.
% Counters are created for all of SubPub (the entire cluster), per node and per subscription.
handle_event({msg_delivered, _MsgId, SubId}, _State) ->
  statsd:increment([list_to_binary("SubPub.msg_delivered"),
  	list_to_binary(string:join(["SubPub", "Nodes", get_node(), "msg_delivered"], ".")),
  	list_to_binary(string:join(["SubPub", "Subscriptions", SubId, "msg_delivered"], "."))]),
  {ok,_State};

% Event handler for incrementing a statsd counter that tracks failed deliveries.
% In this case, a failed delivery is defined as any response from a subscriber endpoint that is not a 200 or 201.
handle_event({delivery_failed, _DeliveryAttemptId, SubId}, _State) ->
  statsd:increment([list_to_binary("SubPub.delivery_failed"),
  	list_to_binary(string:join(["SubPub", "Nodes", get_node(), "delivery_failed"], ".")),
  	list_to_binary(string:join(["SubPub", "Subscriptions", SubId, "delivery_failed"], "."))]),
  {ok,_State};

% Event handler for recording timer events that measure the time elapsed from he point
% the message is recieved from RabbitMQ to the time SubPub receives a response from 
% the subscriber endpoint.
% Timers are created for all of SubPub (entire cluster), per node, and per subscription.
handle_event({msg_delivery_timer, SubId, TimeElapsed}, _State) ->
  statsd:timing([list_to_binary("SubPub.msg_delivery_timer"),
	  list_to_binary(string:join(["SubPub", "Nodes", get_node(), "msg_delivery_timer"], ".")),
	  list_to_binary(string:join(["SubPub", "Subscriptions", SubId, "msg_delivery_timer"], "."))], TimeElapsed, 0.1),
  {ok, _State};

handle_event(_, _State) ->
  {ok,_State}.

%% Not implemented gen_event methods
handle_cast(_Msg, State) -> {noreply, State}.
handle_call(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% Private helper methods
get_node() ->
  [_|[Node|_]] = re:split(atom_to_list(node()), "[@.]", [{return, list}]),
  Node.
