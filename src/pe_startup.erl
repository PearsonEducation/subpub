% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_startup).

-export([start/0]).

-define(DEFAULT_BOOTSTRAP, standalone).

start() ->
  start(get_startup_phases()).

start([]) ->
  ok;

start([Phase|Phases]) ->
  case start_phase(Phase) of
    ok ->
      start(Phases);
    ERROR ->
      ERROR
  end.
  
start_phase({Specs,Callback}) ->
	lists:foreach(
		fun(E) ->
			io:format("Starting ~p...~n",[E]),
			case pe_sup:start_child(E) of
				{ok, Pid} -> {ok, Pid};
				[Error] -> {error, Error}
			end
		end,
		Specs
	),
	call(Callback).

call(undefined) ->
  ok;
call(Callback) ->
  Callback().

get_startup_phases() ->
  EvntMgr = {
              event_manager,
              {event_manager,start_link, []},
              permanent,
              2000,
              worker,
              [dynamic]
           },

  Config = {
              pe_config,
              {pe_config,start_link, []},
              permanent,
              2000,
              worker,
              [pe_config]
           },
  
  Membership = {
              pe_membership,
              {pe_membership,start_link, []},
              permanent,
              2000,
              worker,
              [pe_membership]
           },
           
  IdBroker = {
              pe_id_broker,
              {pe_id_broker,start_link, []},
              permanent,
              2000,
              worker,
              [pe_id_broker]
           },

  Audit = {
              pe_audit,
              {pe_audit,start_link, []},
              permanent,
              2000,
              worker,
              [pe_audit]
           },

  SubStore = {
              pe_sub_store,
              {pe_sub_store,start_link, []},
              permanent,
              2000,
              worker,
              [pe_sub_store]
           },
  
  PrincipalStore = {
              pe_principal_store,
              {pe_principal_store,start_link, []},
              permanent,
              2000,
              worker,
              [pe_principal_store]
           },
  AmqpMgr = {
              amqp_manager,
              {amqp_manager,start_link, []},
              permanent,
              2000,
              worker,
              [amqp_manager]
           },                   
  AmqpBalPub = {
              amqp_balanced_publisher,
              {amqp_balanced_publisher,start_link, []},
              permanent,
              2000,
              worker,
              [amqp_balanced_publisher]
           },                     
  AmqpConsumeMgr = {
              amqp_consume_manager,
              {amqp_consume_manager,start_link, []},
              permanent,
              2000,
              worker,
              [amqp_consume_manager]
           },                     
  Delivery = {
              pe_delivery_pool,
              {pe_delivery_pool,start_link, []},
              permanent,
              2000,
              worker,
              [pe_delivery_pool]
           },                     

  SubIntake = {
              pe_sub_intake,
              {pe_sub_intake,start_link, []},
              permanent,
              2000,
              worker,
              [pe_sub_intake]
           },

  MsgIntake = {
              pe_msg_intake,
              {pe_msg_intake,start_link, []},
              permanent,
              2000,
              worker,
              [pe_msg_intake]
           },           
  
  PrincipalIntake = {
              pe_principal_intake,
              {pe_principal_intake,start_link, []},
              permanent,
              2000,
              worker,
              [pe_principal_intake]
           },           

  Rest = {
              pe_rest,
              {pe_rest,start_link, []},
              permanent,
              2000,
              worker,
              [pe_rest]
           },   

	Admin = {
		pe_admin,
		{ pe_admin, start_link, [] },
		permanent,
		2000,
		worker,
		[ pe_admin ]
	},


  Status = {
              pe_status,
              {pe_status,start_link, []},
              permanent,
              2000,
              worker,
              [pe_status]
           },              

	Collect = {
		pe_collect,
		{pe_collect, start_link, []},
		permanent,
		5000,
		worker,
		[pe_status]
	},
  [           
    {
      [Config, Audit, EvntMgr],
      fun() ->
        post_config_started(),
        post_event_manager_started(),
        ok
      end
    },
    {
      [Membership],
      fun() ->
        post_membership_started()        
      end
    },
    {
      [IdBroker, SubStore, PrincipalStore, Delivery, AmqpMgr, AmqpBalPub, AmqpConsumeMgr],
      fun post_amqp_conn_mgr_started/0
    },
    {
      [SubIntake, MsgIntake, PrincipalIntake],
      fun() ->
        post_sub_proc_mgr_started(),
        ok
      end
    },
    {
      [Status, Rest, Admin, Collect],
      undefined
    }
  ].


get_env(Parent, Key) ->
  {ok, List} = application:get_env(Parent),
  proplists:get_value(Key,List).
  
preset_config(Parent, Key) ->
  ok = pe_config:set(Parent,Key,get_env(Parent,Key)).

post_config_started() ->
  preset_config(admin, file_path),
  preset_config(admin, memory_period),
  preset_config(admin, high_water_mark),
  preset_config(admin, passwd),
  preset_config(admin, port),
  preset_config(admin, private_key),
  preset_config(admin, public_key),
  preset_config(admin, token_threshold),
  preset_config(startup, bootstrap),
  preset_config(startup, join_timeout_millis),
  preset_config(audit,log_path),
  preset_config(audit,include_payload),
  preset_config(delivery, number_of_workers),
  preset_config(delivery, max_failed_attempts),
  preset_config(delivery, callback_timeout),
  preset_config(delivery, ibrowse_max_sessions),
  preset_config(delivery, ibrowse_max_pipeline_size),
  preset_config(delivery, rampdown_multiplier),
  preset_config(delivery, rampdown_interval_seconds),
  preset_config(delivery, rampdown_wait_milliseconds),
  preset_config(amqp,reconnect_wait_millis),
  preset_config(amqp,consumer_prefetch_count),
  preset_config(amqp,confirmed_publishing),
  preset_config(amqp,brokers),
  preset_config(rest,port),
  preset_config(rest,enable_test_services),
  preset_config(rest,secure_error_messages),
  preset_config(rest,status),
  preset_config(rest,server_nickname),
  preset_config(auth,token_date_threshold_milliseconds),
  preset_config(stats,host),
  preset_config(stats,port).
  
post_event_manager_started() ->
  event_manager:add_sup_handler(pe_msg_event_handler),
  event_manager:add_sup_handler(pe_principal_event_handler),
  event_manager:add_sup_handler(pe_stats_event_handler),
  event_manager:add_sup_handler(pe_sub_event_handler).
  
post_membership_started() ->
  Bootstrap = pe_config:get(startup, bootstrap, ?DEFAULT_BOOTSTRAP),
  MYSELF = node(),
  case Bootstrap of 
    standalone ->
      pe_membership:form_new();
    MYSELF ->
      pe_membership:form_new();
    _OTHER ->
      pe_membership:join(Bootstrap)
  end.

post_sub_proc_mgr_started() ->  
  amqp_manager:watch_all(),
  ok.

post_amqp_conn_mgr_started() ->
  post_amqp_conn_mgr_started(pe_config:get(amqp, brokers, [])).

post_amqp_conn_mgr_started([]) ->
  ok;

post_amqp_conn_mgr_started([{Key, Params}|Brokers]) ->
  case amqp_manager:register_broker(Key, amqp_manager:make_broker_params(Key, Params)) of
    ok ->
      amqp_manager:publish_to_broker(Key),
      amqp_manager:consume_from_broker(Key),
      post_amqp_conn_mgr_started(Brokers);
    ERROR ->
      {unable_to_register_broker, {Key, ERROR}}
  end.
