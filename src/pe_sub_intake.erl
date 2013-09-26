% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_sub_intake).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
 
-export([accept/4, delete/1, delete_async/1, start_link/0, delete_all/0]).

-include("include/prospero.hrl").
 
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

accept(Callback, Tags, PrincipalId, WsdlUri) ->
  gen_server:call(?MODULE, {new, Callback, Tags, PrincipalId, WsdlUri}).
  
delete(Subscription) ->
  gen_server:call(?MODULE, {delete, Subscription}).
  
delete_async(Subscription) ->
  gen_server:cast(?MODULE, {delete, Subscription}).
  
delete_all() ->
  gen_server:call(?MODULE, delete_all).
        
init([]) ->
  {
    ok, {pe_sub_intake_state, []}
  }.
  
ensure_minimal_tags(_TagRecords, IsRequireMessageTypeWithNewSubs)  when IsRequireMessageTypeWithNewSubs =:= false ->
  ok;
ensure_minimal_tags(TagRecords, IsRequireMessageTypeWithNewSubs) when IsRequireMessageTypeWithNewSubs =:= true ->
  pe_tag_util:ensure_has_tag(TagRecords, [?MESSAGE_TYPE_TAG_TYPE]).


ensure_no_conflicting_properties("", undefined) -> {incomplete_properties, "callback_url must be specified"};
ensure_no_conflicting_properties("", "") -> {incomplete_properties, "callback_url must be specified"};
ensure_no_conflicting_properties(undefined, undefined) -> {incomplete_properties, "callback_url must be specified"};
ensure_no_conflicting_properties(undefined, "") -> {incomplete_properties, "callback_url must be specified"};

ensure_no_conflicting_properties(undefined, _Callback) -> ok;
ensure_no_conflicting_properties("", _Callback) -> ok;

ensure_no_conflicting_properties(_WsdlUri, _Callback) -> {conflicting_properties, "wsdl_uri is not supported"}.


validate_properties(TagRecords, IsRequireMessageTypeWithNewSubs, WsdlUri, Callback, Principal) ->
  case ensure_minimal_tags(TagRecords, IsRequireMessageTypeWithNewSubs) of
    ok ->
      case ensure_no_conflicting_properties(WsdlUri, Callback) of
        ok ->
          case is_delivery_url_within_principal_mask(Principal, find_url(Callback, WsdlUri)) of
            ok ->
              ok;
            _ELSE ->
              {out_of_mask_url, "wsdl_uri and callback_url must fall within the domain mask configured for the principal"}
          end;
        ELSE ->
          ELSE
      end;
    ELSE ->
      ELSE
  end.

find_url(undefined, WsdlUri) ->
  WsdlUri; 
find_url(CallbackUrl, _) ->
  CallbackUrl.

is_delivery_url_within_principal_mask(Principal, Url) ->
  pe_principal:is_url_within_mask(Url, Principal).
  
strip_wsdl_param(undefined) ->
  undefined;
strip_wsdl_param(Resource) ->
  re:replace(Resource,"\\?wsdl","",[{return, list},caseless]).
 
handle_call({new, CallbackIn, Tags, PrincipalId, WsdlUriIn}, _From, _State) ->
  Callback = pe_util:empty_2_undefined(CallbackIn),
  WsdlUri2 = pe_util:empty_2_undefined(WsdlUriIn),
  WsdlUri = strip_wsdl_param(WsdlUri2),
  
  case pe_principal_store:lookup(PrincipalId) of
    {found, Principal} ->      
      TagRecordsTmp = pe_tag_util:instances(Tags),
      TagRecords = pe_tag_util:inject_tags(pe_principal:get(enforced_tag_ids, Principal), TagRecordsTmp, true),
      
      Result = case validate_properties(TagRecords, pe_principal:get(is_require_message_type_with_new_subs, Principal), WsdlUri, Callback, Principal) of
        ok ->
          TagIds = lists:sort(pe_tag_util:get_tag_ids(TagRecords)),
          DupeKey = pe_sub:make_duplication_key(Callback, WsdlUri, PrincipalId, TagIds),
          
          case pe_sub_store:find_by_duplication_key(DupeKey) of
            [] ->
              {created, Subscription} = pe_sub_store:create_new(pe_sub:instance(undefined, PrincipalId, Callback, WsdlUri, undefined, TagIds, undefined, DupeKey)),
              
              Tmp2 = pe_tag_util:make_tag_audit_log_tags(TagRecords),
              Tmp3 = [{sub,pe_sub:get(id, Subscription)},{callback_url,Callback},{wsdl_uri,WsdlUri},{principal, PrincipalId}],
              
              pe_audit:log(lists:append(Tmp3,Tmp2),"SUBSCRIPTION"),

              event_manager:notify({sub_created, Subscription}),
              {ok,Subscription};
            [DupedSub] ->
              {invalid_request, {duplicate_subscription, pe_sub:get(id, DupedSub)}};
            [DupedSub|_OtherDupedSubs] ->
              {invalid_request, {duplicate_subscription, pe_sub:get(id, DupedSub)}}
          end;
        {conflicting_properties, Desc} ->
          {invalid_request, {conflicting_properties, Desc}};
        {incomplete_properties, Desc} ->
          {invalid_request, {incomplete_properties, Desc}};
        {missing, Tag} ->
          {invalid_request, {missing_tag, Tag}};
        {out_of_mask_url, Error} ->
          {invalid_request, {out_of_mask_url, Error}};
        UNRECOGNIZED ->
          error_logger:error_msg("pe_sub_intake: Unrecognized result from validate_properties(): ~p~n", [UNRECOGNIZED]),
          error
      end,
      {reply,Result,_State};
    none ->
      {reply, {error, principal_not_found},_State}
  end;
  
handle_call({delete, SubscriptionId}, _From, _State) when is_list(SubscriptionId) ->
  case pe_sub_store:lookup(SubscriptionId) of
    {found, Subscription} ->
      do_delete(Subscription),
      {reply,{ok,Subscription},_State};
    _ERROR ->
      {reply,{error, unable_to_find_subscription},_State}
  end;

handle_call({delete, Subscription}, _From, State) ->
  {Result, State} = handle_delete(Subscription, State),
  {reply, Result, State};
  
handle_call(delete_all, _From, _State) ->
  Ids = pe_sub_store:get_all_ids(),
  lists:foreach(
    fun(Id) ->
      {found, Sub} = pe_sub_store:lookup(Id),
      do_delete(Sub)
    end,
    Ids
  ),
  {reply,ok,_State}.


do_delete(Subscription) ->
  %% CKC: Note that we're not calling `event_manager:notify({sub_cancelled, Subscription})` because pe_sub_store does this for us based on an mnesia table event
  {cancelled,_Sub} = pe_sub_store:cancel(Subscription),
  pe_audit:log([{sub,pe_sub:get(id, Subscription)}],"SUBSCRIPTION-CANCELLED").
    
handle_delete(SubscriptionId, _State) when is_list(SubscriptionId) ->
  case pe_sub_store:lookup(SubscriptionId) of
    {found, Subscription} ->
      handle_delete(Subscription, _State);
    _ERROR ->
      {{error, unable_to_find_subscription},_State}
  end;

handle_delete(Subscription, _State) ->
  do_delete(Subscription),
  {{ok,Subscription},_State}.

handle_cast({delete, Subscription}, State) ->
  {_Result, State} = handle_delete(Subscription, State),
  {noreply, State};
  
handle_cast(_Msg, State) ->
  error_logger:warning_msg("pe_sub_intake: Received unkown handle_cast: ~p~n",[_Msg]),  
  {noreply, State}.

handle_info(_Msg, State) ->
  error_logger:warning_msg("pe_sub_intake: Received unknown handle_info: ~p~n",[_Msg]),  
  {noreply, State}.


terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
