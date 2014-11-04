-module(pe_collect).
-behavior(gen_server).

%%
%% a gen_server that periodically checks to see if memory consumption is above a
%% certain point.  If it is then garbage collection is run on all the processes in
%% the system.
%%
%% The /prospero/admin/memory_period is the period, in milliseconds, between
%% checks.  This value defaults to 60 seconds.
%% 
%% The /prospero/admin/high_water_mark is the amount of memory in bytes that
%% erlang:memory/0 needs to indicate is used before garbage collection is run.
%%
%% The system works by accepting a message that is periodically sent to the
%% gen_server via the timer:send_interval function.
%% 

-export([
	check/0,
	code_change/3,
	collect/0,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	init/1,
	start_link/0,
	start_link/1,
	terminate/2
]).

-define(DEFAULT_PERIOD, 60000).
-define(DEFAULT_HIGH_WATER_MARK, 8000000000).
-record(state, {high_water_mark}).

period() ->
	pe_config:get(admin, memory_period, ?DEFAULT_PERIOD).

high_water_mark() ->
	pe_config:get(admin, high_water_mark, ?DEFAULT_HIGH_WATER_MARK).

check() ->
	gen_server:call(?MODULE, check).

collect() ->
	gen_server:call(?MODULE, collect).

%
% start up the server with the default values
%
start_link() ->
	Args = [
		{ period, period()},
		{ high_water_mark, high_water_mark()}
	],
	start_link(Args).

%
% start up the server with a specified set of values.  The arguments are period
% for the period between checks and high_water_mark for the number of bytes used.
%
start_link(Args) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%
% get one of the control parameters, using the default value if the parameter is 
% not present in the list.
%
get_param (Name, []) ->
	case Name of
		period -> period();
		high_water_mark -> high_water_mark()
	end;
get_param (Name, [ { Name, Value } | _Tail ]) ->
	Value;
get_param (Name, [ _Other | Tail ]) ->
	get_param(Name, Tail).

%
% start up the server
%
init(Args) ->
	Period = get_param(period, Args),
	timer:send_interval(Period, interval),
	HighWaterMark = get_param(high_water_mark, Args),
	% io:format("period ~p, HWM ~p", [Period, HighWaterMark]),
	{ok, #state{high_water_mark = HighWaterMark}}.

%
% extract the memory currently allocated to the system.  
%
get_allocated_memory([ { total, Total } | _Tail ]) ->
	Total;
get_allocated_memory([_Other | Tail]) ->
	get_allocated_memory(Tail).

%
% perform a check at a client's request
%
handle_call(check, _From, State) ->
	do_check(State),
	{reply, ok, State};

%
% force a garbage collection at the request of a client
%
handle_call(collect, _From, State) ->
	do_collect(),
	{reply, ok, State}.

%
% periodically check the state of system memory
%
handle_info(interval, State) ->
	do_check(State),
	{noreply, State}.

%
% check to see if garbage collection should be run.  If we have reached the
% high watever mark, then run garbage collection.
%
do_check(State) ->
	AllocatedMemory = get_allocated_memory(erlang:memory()),
	Alloc = trunc(AllocatedMemory/(1024 * 1024)),
	HWM = trunc(State#state.high_water_mark/(1024*1024)),
	pe_audit:log([{allocated, Alloc}, {high_water_mark, HWM}], "GC-CHECK"),

	case AllocatedMemory >= State#state.high_water_mark of
		true -> do_collect();
		_ -> ok
	end.

%
% run garbage collection on all processes in the system.
%
do_collect() ->
    pe_audit:log([],"GC-COLLECT"),
	lists:map(
		fun(Pid) ->
			erlang:garbage_collect(Pid)
		end,
		processes()
	).

%
% functions required by gen_server
%
code_change(_Old, State, _Extra) ->
	{ok, State}.

handle_cast(_Message, State) ->
	{noreply, State}.

terminate(shutdown, _State) ->
	ok.
