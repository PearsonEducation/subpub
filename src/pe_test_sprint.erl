% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_test_sprint).

-export([setup/0, test/1, go/1]).
-export([handle_event/2, init/1, terminate/2]).


init([]) ->
  {ok, []}.
  

go(TargetDuration) ->
  pe_test_sprint:setup(),
  pe_test_sprint:test(TargetDuration).

setup() ->
  ets:new(pe_test_sprint,[set,public,named_table,{keypos,1},{heir,none},{write_concurrency,false}]),
  {ok,_Sub3} = pe_sub_intake:accept("http://localhost/index.php/erlang_sprint3/200/REF12347/MESSAGE-ID",["EP:Kaplan","System:Threads"],undefined,"11111111","SprintUser1"),
  {ok,_Sub2} = pe_sub_intake:accept("http://localhost/index.php/erlang_sprint2/200/REF12346/MESSAGE-ID",["EP:Kaplan","System:Threads"],undefined,"11111112","SprintUser1"),
  {ok,_Sub1} = pe_sub_intake:accept("http://localhost/index.php/erlang_sprint1/200/REF12345/MESSAGE-ID",["EP:Kaplan","System:Threads"],undefined,"11111113","SprintUser1").

test(TargetDuration) ->
  event_manager:add_handler(?MODULE),
  Start = get_current_seconds(),
  ets:insert(pe_test_sprint, {pe_test_sprint_msg_count, 1}),
  NumSent = loop(0, TargetDuration, Start),
  
  delivery_wait_loop(NumSent, get_num_del_orders(), TargetDuration, Start),
  event_manager:delete_handler(?MODULE).
  
delivery_wait_loop(NumSent, NumDelivered, Duration, Start) when NumDelivered < NumSent * 3 ->
  print_scoreboard(NumSent,NumDelivered,Duration),
  timer:sleep(1000),
  delivery_wait_loop(NumSent, get_num_del_orders(), get_current_seconds() - Start, Start);

delivery_wait_loop(NumSent, NumDelivered, Duration, _Start) ->
  print_scoreboard(NumSent,NumDelivered,Duration),
  io:format("That is ~p messages per second!~n",[NumDelivered / Duration]).
  
print_scoreboard(NumSent,NumDelivered,Duration) ->
  io:format("Sent ~p messages and ordered ~p for delivery in ~p seconds~n",[NumSent,NumDelivered,Duration]).
  
get_current_seconds() ->
  calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(erlang:now())).


loop(NumSent, TargetDuration, Start) ->
  loop(NumSent, TargetDuration, Start, 0).
  
loop(NumSent, TargetDuration, Start, Duration) when Duration < TargetDuration ->
  Payload = lists:append("This is a message ", integer_to_list(NumSent)),
  {ok,_MsgId} = pe_msg_intake:accept(list_to_binary(Payload), "MessageType1", ["EP:Kaplan","System:Threads"], "SprintUser1"),
  loop(NumSent + 1, TargetDuration, Start, get_current_seconds() - Start);
  
loop(NumSent, _TargetDuration, _Start, _Duration) ->
  NumSent.
  
get_num_del_orders() ->
  [{pe_test_sprint_msg_count,Count}] = ets:lookup(pe_test_sprint,pe_test_sprint_msg_count),
  Count - 1.

handle_event({msg_delivered, _MsgId, _SubId}, _State) ->
  ets:update_counter(pe_test_sprint,pe_test_sprint_msg_count,1),
  {ok,_State};

handle_event(_, _State) ->
  {ok,_State}.

terminate(_Reason, _State) -> ok.

