%%%-------------------------------------------------------------------
%%% File    : eb_event_manager.erl
%%% Author  : Mitchell Hashimoto <mitchellh@Chip.local>
%%% Description :
%%%
%%% Created :  7 Sep 2008 by Mitchell Hashimoto <mitchellh@Chip.local>
%%%-------------------------------------------------------------------
-module(event_manager).

%% API
-export([start_link/0, add_handler/1, add_handler/2, delete_handler/1, add_sup_handler/1, notify/1, sync_notify/1]).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | {error,Error}
%% Description: Creates an event manager.
%%--------------------------------------------------------------------
start_link() ->
  gen_event:start_link({local, ?SERVER}). 

%%--------------------------------------------------------------------
%% Function: add_handler(Module) -> ok | {'EXIT',Reason} | term()
%% Description: Adds an event handler
%%--------------------------------------------------------------------
add_handler(Module) ->
  gen_event:add_handler(?SERVER, Module, []).
add_handler(Module,Args) ->
  gen_event:add_handler(?SERVER, Module, [Args]).

delete_handler(Module) ->
  gen_event:delete_handler(?SERVER, Module, []).

%%--------------------------------------------------------------------
%% Function: add_sup_handler(Module) -> ok | {'EXIT',Reason} | term()
%% Description: Adds a supervised event handler
%%--------------------------------------------------------------------
add_sup_handler(Module) ->
  gen_event:add_sup_handler(?SERVER, Module, []).

%%--------------------------------------------------------------------
%% Function: notify(Event) -> ok | {error, Reason}
%% Description: Sends the Event through the event manager.
%%--------------------------------------------------------------------
notify(Event) ->
  gen_event:notify(?SERVER, Event).
  
sync_notify(Event) ->
  gen_event:sync_notify(?SERVER, Event).

