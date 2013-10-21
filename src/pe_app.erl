% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.



-module(pe_app).

-behaviour(application).

-export([start/2,stop/1,go/0,stop_and_halt/0]).

-define(APP_NAME,prospero).

alert_started(Service) ->
  error_logger:info_msg("STARTED prospero service ~p~n",[Service]).
  
start_application([Service|Remaining]) ->
  case application:start(Service) of
    ok ->
      alert_started(Service),
      start_application(Remaining);
    Error ->
      error_logger:error_msg("An error occurred while starting ~s: ~p~n",[Service, Error]),
      {error, {Service, Error}}
  end;

start_application([]) ->
  ok.
  
get_apps_to_start("R13" ++ _Revision) ->
  [crypto, ssl, inets, ibrowse];

get_apps_to_start("R16" ++ _Revision) ->
  [crypto, asn1, public_key, ssl, inets, ibrowse];

get_apps_to_start(_Revision) ->
  [crypto, public_key, ssl, inets, ibrowse].


start(_Type, _StartArgs) ->
  case start_application( get_apps_to_start(erlang:system_info(otp_release)) ) of
    ok ->
      case pe_sup:start_link() of
        {ok, Pid} ->
          case pe_startup:start() of
            ok ->
              {ok, Pid};
            Error ->
              error_logger:error_msg("An error occurred while starting Prospero services: ~p~n",[Error]),
              {error, Error}
          end;
        Error ->
          error_logger:error_msg("An error occurred while starting Supervisor: ~p~n",[Error]),
          {error, Error}
      end;
    Error ->
      error_logger:error_msg("An error occurred while starting dependancy services: ~p~n",[Error]),
      {error, Error}
  end.  
  
stop(_State) ->
  ok.

go() ->
  case application:start(?APP_NAME) of
    ok ->
      error_logger:info_msg("Prospero is up and running as node ~p~n~n~n",[node()]),
      mnesia:info(),
      ok;
    Error ->
      io:format("Prospero could not be started: ~p",[Error]),
      error_logger:error_msg("An error occurred while starting Prospero: ~p~n",[Error]),
      timer:sleep(1000), %Give SASL a second to print the error before halting
      halt(1)
  end.

stop_and_halt() ->
  case application:stop(prospero) of
    ok ->
      case application:stop(ibrowse) of
        ok ->
          case application:stop(crypto) of
            ok ->
              ok;
            Error ->
              error_logger:error_msg("An error occurred while stopping crypto: ~p~n",[Error]),
              Error
          end;
        Error ->
          error_logger:error_msg("An error occurred while stopping ibrowse: ~p~n",[Error]),
          Error
      end;
    Error ->
      error_logger:error_msg("An error occurred while stopping Prospero: ~p~n",[Error]),
      Error
  end,
  init:stop().


