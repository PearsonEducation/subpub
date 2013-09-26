% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.


-module(pe_audit).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). 
-export([log/2, start_link/0, reset/0, test/1]).

-record(pa_state, {
  handle,
  queue,
  flush_requested = false,
  newline_re,
  dos_newline_re
}).

log(Args,Msg) ->
  gen_server:call(?MODULE, {log, Args, Msg, pe_time:format_8601()}).
  
reset() ->
  gen_server:call(?MODULE, reset).


start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

acquire_handle() ->
  Path = get_log_path(),
  {ok, Handle} = file:open(Path, [append]),
  {Handle, Path}.
  
init([]) ->
  {ok, ReNewLine} = re:compile("\n",[multiline]),
  {ok, ReDosNewLine} = re:compile("\r\n",[multiline]),

  {ok, #pa_state{
    handle=undefined, 
    queue=queue:new(), 
    newline_re=ReNewLine,
    dos_newline_re=ReDosNewLine
  }}.

get_log_path() ->
  pe_config:get(audit,log_path,"audit.log").

format_args(Args, ReNewline, ReDosNewLine) ->
  format_args(Args, "", "", ReNewline, ReDosNewLine).
  
format_args([{Key,Value}|T], Result, Delim, ReNewline, ReDosNewLine) ->
  NewPiece = lists:flatten(io_lib:format("~s~s=~s",[Delim,to_string(Key, ReNewline, ReDosNewLine),to_string(Value, ReNewline, ReDosNewLine)])),
  format_args(T, string:concat(Result, NewPiece), " ", ReNewline, ReDosNewLine);

format_args([], Result, _Delim, _ReNewline, _ReDosNewLine) ->
  Result.
  
clean(Value, ReNewline, ReDosNewLine) ->
  Tmp = re:replace(Value,ReDosNewLine,"",[{return, list}]),
  re:replace(Tmp,ReNewline,"",[{return, list}]).

to_string(Value, ReNewline, ReDosNewLine) when is_list(Value) ->
  clean(Value, ReNewline, ReDosNewLine);
to_string(Value, ReNewline, ReDosNewLine) ->
  clean(io_lib:format("~p",[Value]), ReNewline, ReDosNewLine).

%%
%% Logs messages one per line in the format:
%% <date_time_utc> - <node> - <Msg> - <Args>
%%
%% Args - an array of [<key>:<value>] pairs surrounded in brackets and separated with a comma and a space.
%%
%% Example:
%% 2010-04-05 22:05:06Z CONFIG_PROPERTY [section:amqp] [key:exchange] [new_value:<<"prospero">>] [old_value:undefined]
%%%%%io:format(Handle, "~s ~s ~s~n", [pe_time:format_8601(),Msg,format_args(Args)]),

do_flush_item_recursive({{value, {Args, Msg, Time}}, Queue}, Handle, ReNewline, ReDosNewLine) ->
  LineArgs = [Time,Msg,format_args(Args, ReNewline, ReDosNewLine)],
  io:format(Handle, "~s ~s ~s~n", LineArgs),
  do_flush_item_recursive(queue:out(Queue), Handle, ReNewline, ReDosNewLine);

do_flush_item_recursive({empty, Queue}, _Handle, _ReNewline, _ReDosNewLine) ->
  Queue.

do_flush(Queue, Handle, ReNewline, ReDosNewLine) ->
  do_flush_item_recursive(queue:out(Queue), Handle, ReNewline, ReDosNewLine).

handle_cast(flush, #pa_state{handle=undefined} = State) ->
  gen_server:cast(?MODULE, flush),
  {noreply, State#pa_state{handle=acquire_handle()}};

handle_cast(flush, #pa_state{queue=Queue, handle={Handle, _Path}, newline_re=ReNewline, dos_newline_re=ReDosNewLine} = State) ->
  NewQueue = do_flush(Queue, Handle, ReNewline, ReDosNewLine),
  {noreply, State#pa_state{queue=NewQueue, flush_requested=false}}.

handle_call({log, Args, Msg, Time}, _From, #pa_state{handle=HandleIn, newline_re=ReNewline, dos_newline_re=ReDosNewLine} = State) ->
  %NewQueue = queue:in({Args, Msg, Time}, Queue),

  %case FlushRequested of
  %  true -> ok;
  %  false -> gen_server:cast(?MODULE, flush)
  %end,

  {Handle, _Path} = NewHandle = case HandleIn of
    undefined ->
      acquire_handle();
    _HANDLE ->
      _HANDLE
  end,

  LineArgs = [Time,Msg,format_args(Args, ReNewline, ReDosNewLine)],
  io:format(Handle, "~s ~s ~s~n", LineArgs),  

  {reply, ok, State#pa_state{handle=NewHandle}};
 
handle_call(reset, _From, #pa_state{handle={Handle, _Path}} = State) ->
  file:close(Handle),
  {NewHandle, NewPath} = acquire_handle(),
  error_logger:info_msg("Reseting audit log: acquired new file handle at path ~s~n",[NewPath]),
  {reply, ok, State#pa_state{handle={NewHandle, NewPath}}}.
  
handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, #pa_state{handle=undefined}) ->
  ok;

terminate(_Reason, #pa_state{handle={Handle, Path}}) ->
  error_logger:info_msg("Closing audit log file handle at path ~s~n",[Path]),
  file:close(Handle),
  ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

test(0) ->
  ok;

test(Num) ->
  log([{time, Num},{blah,"Foo"}],"TESTITEM"),
  test(Num-1).
