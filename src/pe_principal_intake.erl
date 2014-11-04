% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_principal_intake).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
 
-export([
	accept/8, 
	activate/1, 
	deactivate/1, 
	start_link/0
]).

-include("include/prospero.hrl").
 
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

accept(PrincipalId, FriendlyName, EnforcedTags, RequireMessageTypeWithSubscription, Secret, Realm, DeliveryUrlMask, DurableMessagingEnabled) ->
  gen_server:call(?MODULE, {new, PrincipalId, FriendlyName, EnforcedTags, RequireMessageTypeWithSubscription, Secret, Realm, DeliveryUrlMask, DurableMessagingEnabled}).
  
deactivate({id, PrincipalId}) ->
  {found, Principal} = pe_principal_store:lookup(PrincipalId),
  gen_server:call(?MODULE, {deactivate, Principal});

deactivate({pe_principal, Principal}) ->
  gen_server:call(?MODULE, {deactivate, Principal});

deactivate(Principal) ->
  deactivate({pe_principal, Principal}).

activate({id, PrincipalId}) ->
  {found, Principal} = pe_principal_store:lookup(PrincipalId),
  gen_server:call(?MODULE, {activate, Principal});

activate({pe_principal, Principal}) ->
  gen_server:call(?MODULE, {activate, Principal});

activate(Principal) ->
  activate({pe_principal, Principal}).



init([]) ->
  {ok, []}.
  

create_secret_or_use_provided(undefined) ->
  pe_util:generate_key();
  
create_secret_or_use_provided(Secret) ->
  Secret.
  
principal_exists(PrincipalId) ->
  case pe_principal_store:lookup(PrincipalId) of
    none ->
      false;
    {found, _Found} ->
      true
  end.

handle_call({new, PrincipalId, FriendlyName, EnforcedTags, RequireMessageTypeWithSubscription, Secret, Realm, DeliveryUrlMask, DurableMessagingEnabled}, _From, _State) ->
  TagRecords = pe_tag_util:instances(EnforcedTags),

  AssignedSecret = create_secret_or_use_provided(Secret),
  
  Result = case principal_exists(PrincipalId) of
    false ->
      {created, Principal} = pe_principal_store:create_new(
        pe_principal:instance(PrincipalId, FriendlyName, pe_tag_util:get_tag_ids(TagRecords), RequireMessageTypeWithSubscription, AssignedSecret, undefined, Realm, DeliveryUrlMask, DurableMessagingEnabled)
      ),
  
      Tmp2 = pe_tag_util:make_tag_audit_log_tags(TagRecords),
      Tmp3 = [{principal,PrincipalId},{friendly_name, FriendlyName},{is_require_message_type_with_new_subs, RequireMessageTypeWithSubscription},{durable_messaging_enabled, DurableMessagingEnabled},{realm, Realm}],
      
      pe_audit:log(lists:append(Tmp3,Tmp2),"PRINCIPAL"),

      event_manager:notify({principal_created, Principal}),
      {ok,Principal};
    true ->
      {error, duplicate_principal_id}
  end,
  {reply,Result,_State};
  
handle_call({activate, Principal}, _From, _State) ->
  event_manager:notify({principal_deactivated, Principal}),

  {cancelled,_Principal} = pe_principal_store:activate(Principal),

  pe_audit:log([{principal, pe_principal:get(id,Principal)}],"PRINCIPAL-ACTIVATED"),
  {reply,{ok,_Principal},_State};

handle_call({deactivate, Principal}, _From, _State) ->
  event_manager:notify({principal_deactivated, Principal}),

  {cancelled,_Principal} = pe_principal_store:deactivate(Principal),

  pe_audit:log([{principal, pe_principal:get(id,Principal)}],"PRINCIPAL-DEACTIVATED"),
  {reply,{ok,_Principal},_State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
