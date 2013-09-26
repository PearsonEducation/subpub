-module(pe_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/2, start_child/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Key, Value) ->
  supervisor:start_child(?SERVER, [Key, Value]).

start_child(Spec) ->
  supervisor:start_child(?SERVER, Spec).  
  
init([]) ->
  {ok, {{one_for_one,1,1}, []}}.
  
  
  
  
