% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_rest).

-export([start_link/0, server_in/0, server_out/0]).

-record(pe_rest_response, {status=200, body="", content_type="text/plain", headers=[]}).
-include("include/prospero.hrl").

-define(DEFAULT_PORT,8080).
-define(AUTH_HEADER_KEY,"Authorization").
-define(AUTH_DELIMITER_KEY,"AuthorizationDelimiter").

-define(DEFAULT_REST_SECURE_ERROR_MESSAGES, false).
-define(DEFAULT_CONFIRMED_PUBLISHING, false).

start_link() ->
  Loop = fun(Req) ->
    try handle_request(Req, "v1") of
      Val -> Val
    catch
      _ErrorType:Error ->
        error_logger:error_msg("Unable to complete Request: ~p~n~nRequest Dump:~n~p~n~nStack Trace:~n~p~n", [Error,Req:dump(),erlang:get_stacktrace()]),
        send_response(Req, #pe_rest_response{status=500, content_type="text/plain", body=io_lib:format("A problem occurred: ~p",[Error])})
    end
  end,
  
  Port = pe_config:get(rest,port,?DEFAULT_PORT),
  
  {ok, Pid} = case mochiweb_http:start([
    {loop, Loop},
    {name, ?MODULE},
    {port, Port}
  ]) of
    {ok, MochiPid} -> {ok, MochiPid}
  end,
  error_logger:info_msg("Started mochiweb with pid ~p on port ~p~n",[Pid, Port]),
  {ok, Pid}.

add_force_close_header(Headers) ->
  case pe_config:get(rest,status,undefined) of
    serverin ->
      Headers;
    serverout ->
      [{"Connection","close"}|Headers]
  end.

obfuscate_error_message(Status, Body, false) when Status > 399 ->
  case pe_config:get(rest, secure_error_messages, ?DEFAULT_REST_SECURE_ERROR_MESSAGES) of
    false ->
      Body; %We're not obfuscating error messages, so return the original body
    _ELSE ->
      case Status of
        409 ->
          Body; %It's benign to return a duplicate message
        _ELSE2 ->
          case Status > 399 andalso Status < 500 of
            true ->
              "Invalid request";
            false ->
              "An error occurred while processing the request"
          end
      end
  end;
  
obfuscate_error_message(_Status, Body, _OverrideSecureMessage) ->
  Body.
  
  
log_invalid_request(Status, Req, Body) when Status > 399 andalso Status < 500 ->
  error_logger:warning_msg("INVALID REQUEST~nStatus: ~p~nBody: ~p~nPeer: ~p~n~nRequest Dump:~n~p~n", [Status, lists:flatten(Body), Req:get(peer), Req:dump()]);
  
log_invalid_request(_Status, _Req, _Body) ->
  void.

send_response(Req, Response) ->
  send_response(Req, Response, false).

send_response(Req, #pe_rest_response{status=Status, body=Body, content_type=ContentType, headers=Headers}, OverrideSecureMessage) ->
  event_manager:notify({send_response, Status}),
  log_invalid_request(Status, Req, Body),
  NewBody = obfuscate_error_message(Status, Body, OverrideSecureMessage),
  Headers2 = lists:append(Headers, [{"Content-Type",ContentType},{"Server",pe_config:get(rest,server_nickname,node())}]),         
  Req:respond({Status,add_force_close_header(Headers2),NewBody}).
  
bin(Val) ->
  pe_util:to_binary(Val).
  
server_in() ->
  pe_config:set(rest,status,serverin).
  
server_out() ->
  pe_config:set(rest,status,serverout).

handle_request(Req, PathPrefix) ->
  [PathPrefix|[Resource2|_]] = string:tokens(Req:get(path),"/"),
  event_manager:notify({handle_request, Resource2}),
  case string:tokens(Req:get(path),"/") of
    [PathPrefix,"subscription",Id] ->
      case Req:get(method) of
        'GET' ->
          send_response(Req, handle_get_subscription(Req, Id));
        'DELETE' ->
          send_response(Req, handle_delete_subscription(Req, Id));
        _ ->
          send_response(Req, #pe_rest_response{status=405, body="Method Not Allowed"})
      end;
    [PathPrefix,"subscriptions","principal", PrincipalId] ->
      case Req:get(method) of
        'GET' ->
          send_response(Req, handle_get_subscriptions_for_principal(Req, PrincipalId));
        _ ->
          send_response(Req, #pe_rest_response{status=405, body="Method Not Allowed"})
      end;
    [PathPrefix,"subscriptions","principal", PrincipalId, "active"] ->
      case Req:get(method) of
        'GET' ->
          send_response(Req, handle_get_active_subscriptions_for_principal(Req, PrincipalId));
        _ ->
          send_response(Req, #pe_rest_response{status=405, body="Method Not Allowed"})
      end;
    [PathPrefix,"subscription"] ->
      case Req:get(method) of
        'POST' ->
          send_response(Req, handle_post_subscription(Req));
        _ ->
          send_response(Req, #pe_rest_response{status=405, body="Method Not Allowed"})
      end;
    [PathPrefix,"message"] ->
      case Req:get(method) of
        'POST' ->
          send_response(Req, handle_post_message(Req));
        _ ->
          send_response(Req, #pe_rest_response{status=405, body="Method Not Allowed"})
      end;
    [PathPrefix,"status"] ->
      case Req:get(method) of
        'GET' ->
          send_response(Req, handle_status(Req));
        _ ->
          send_response(Req, #pe_rest_response{status=405, body="Method Not Allowed"})
      end;
    [PathPrefix,"status","simple"] ->
      case Req:get(method) of
        'GET' ->
          send_response(Req, handle_status_simple(Req), true);
        _ ->
          send_response(Req, #pe_rest_response{status=405, body="Method Not Allowed"})
      end;
    [PathPrefix,"test","broker",Op,Key] ->
      case pe_config:get(rest,enable_test_services,false) of
        true ->
          case Req:get(method) of
            'POST' ->
              case Op of
                "publish" ->
                  case amqp_manager:publish_to_broker(Key) of
                    ok -> send_response(Req, #pe_rest_response{body="OK"});
                    ERROR -> send_response(Req, #pe_rest_response{status=500, body=io_lib:format("~s",[ERROR])})
                  end;
                "consume" ->
                  case amqp_manager:consume_from_broker(Key) of
                    ok -> send_response(Req, #pe_rest_response{body="OK"});
                    ERROR -> send_response(Req, #pe_rest_response{status=500, body=io_lib:format("~s",[ERROR])})
                  end;
                _ ->
                  send_response(Req, #pe_rest_response{status=404,body="Unknown broker operation"})
              end;
            'DELETE' ->
              case Op of
                "publish" ->
                  amqp_manager:stop_publishing_to_broker(Key),
                  send_response(Req, #pe_rest_response{body="OK"});
                "consume" ->
                  amqp_manager:stop_consumers_at_broker(Key),
                  send_response(Req, #pe_rest_response{body="OK"});
                _ ->
                  send_response(Req, #pe_rest_response{status=404,body="Unknown broker operation"})
              end;
            _ ->
              send_response(Req, #pe_rest_response{status=405, body="Method Not Allowed"})
          end;
        false ->
          send_response(Req, #pe_rest_response{status=404,body="Unknown resource pattern"})
      end;
    [PathPrefix,"test",Resource] ->
      case pe_config:get(rest,enable_test_services,false) of
        true ->
          case Resource of
            "create_signature" ->
              case Req:get(method) of
                'POST' ->
                  send_response(Req, handle_create_signature(Req));
                _ ->
                  send_response(Req, #pe_rest_response{status=405, body="Method Not Allowed"})
              end;
            "clear_subscriptions" ->
              case Req:get(method) of
                _ ->
                  send_response(Req, #pe_rest_response{status=405, body="Method Not Allowed"})
              end;
            "serverin" ->
              case Req:get(method) of
                'POST' ->
                  server_in(),
                  send_response(Req, #pe_rest_response{body="OK"});
                _ ->
                  send_response(Req, #pe_rest_response{status=405, body="Method Not Allowed"})
              end;
            "serverout" ->
              case Req:get(method) of
                'POST' ->
                  server_out(),
                  send_response(Req, #pe_rest_response{body="OK"});
                _ ->
                  send_response(Req, #pe_rest_response{status=405, body="Method Not Allowed"})
              end;
            "secure_messages" ->
              case Req:get(method) of
                'POST' ->
                  pe_config:set(rest,secure_error_messages,true),
                  send_response(Req, #pe_rest_response{body="OK"});
                _ ->
                  send_response(Req, #pe_rest_response{status=405, body="Method Not Allowed"})
              end;
            "insecure_messages" ->
              case Req:get(method) of
                'POST' ->
                  pe_config:set(rest,secure_error_messages,false),
                  send_response(Req, #pe_rest_response{body="OK"});
                _ ->
                  send_response(Req, #pe_rest_response{status=405, body="Method Not Allowed"})
              end;
            _ ->
              send_response(Req, #pe_rest_response{status=404,body="Unknown resource pattern"})
          end;
        false ->
          send_response(Req, #pe_rest_response{status=404,body="Unknown resource pattern"})
      end;
    _ ->
      send_response(Req, #pe_rest_response{status=404,body="Unknown resource pattern"})

  end.

get_authorization_from_header(Req) ->
  {Req:get_header_value(?AUTH_HEADER_KEY), Req:get_header_value(?AUTH_DELIMITER_KEY)}.

get_authorization_from_url(Req) ->
  Params = Req:parse_qs(),
  {proplists:get_value(?AUTH_HEADER_KEY, Params), proplists:get_value(?AUTH_DELIMITER_KEY, Params)}.
  
get_authorization(Req, Auth, Delimiter) ->
  case get_authorization_from_header(Req) of
    {undefined, _Delim1} ->
      case get_authorization_from_url(Req) of
        {undefined, _Delim2} ->
          {Auth, Delimiter};
        Value ->
          Value
      end;
    Value ->
      Value
  end.

default_delimiter(undefined) ->
  "|";
default_delimiter(Val) ->
  Val.
  
authenticate(Req, Auth, Payload, Delimiter) ->
  case get_authorization(Req, Auth, Delimiter) of
    {undefined, _Delim} ->
      {error, authorization_token_not_provided};
    {AuthToken, Delim2} ->
      case pe_util:validate_all_inputs([
        {AuthToken, authorization, auth},
        {Delim2, authorization_delimiter, auth_delimiter}
      ]) of
        valid ->
          pe_auth:authenticate(AuthToken, Payload, pe_principal_store, default_delimiter(Delim2));
        {invalid, Desc} ->
          {not_authenticated, "Invalid authorization input format: " ++ io_lib:format("~p",[Desc])}
      end
  end.

get_tag_structs(Ids) ->
  get_tag_structs(pe_tag_util:instances(Ids), []).

get_tag_structs([Tag|Tail], Result) ->
  get_tag_structs(Tail, [get_tag_struct(Tag)|Result]);

get_tag_structs([], Result) ->
  Result.

get_tag_struct(Tag) ->
  {struct, [
    {tag,
      {struct, [
        {id,bin(Tag#pe_tag.id)},
        {type,bin(Tag#pe_tag.type)},
        {value,bin(Tag#pe_tag.value)}
      ]}
    }
  ]}.
  
get_subscription_struct(Subscription) ->
  {struct, [
    {subscription,
      {struct, [
        {id,bin(pe_sub:get(id,Subscription))},
        {principal_id,bin(pe_sub:get(principal_id,Subscription))},
        {callback_url,bin(pe_sub:get(callback_url,Subscription))},
        {wsdl_uri,bin(pe_sub:get(wsdl_uri,Subscription))},
        {queue_name,bin(pe_sub:get(queue_name,Subscription))},
        {date_created,bin(pe_time:format_8601(pe_sub:get(date_created, Subscription)))},
        {date_cancelled,bin(pe_time:format_8601(pe_sub:get(date_cancelled, Subscription)))},
        {tags, get_tag_structs(pe_sub:get(tag_ids,Subscription))}
      ]}
    }
  ]}.

get_subscriptions_struct(Subscriptions) ->
  {struct, [
    {subscriptions, lists:map(fun(Subscription) -> get_subscription_struct(Subscription) end, Subscriptions)}
  ]}.
    
handle_get_subscriptions_for_principal(Req, PrincipalIdInput) ->
  case pe_util:validate_input(normal, PrincipalIdInput) of
    valid ->
      case authenticate(Req, undefined, PrincipalIdInput, undefined) of
        {authenticated_ok, PrincipalId} ->
          case PrincipalIdInput == PrincipalId of
            true ->
              Subs = pe_sub_store:get_all_for_principal(PrincipalId),
              #pe_rest_response{content_type="application/json", body=mochijson2:encode(get_subscriptions_struct(Subs))};
            false ->
              #pe_rest_response{status=401, content_type="text/plain", body=io_lib:format("Principal Id from the Authorization token (~s) does not match the one provided in the URL (~s).",[PrincipalId, PrincipalIdInput])}
          end;        
        {error, authorization_token_not_provided} ->
          #pe_rest_response{status=401, content_type="text/plain", body="An authorization token must be provided by including the Authorization request header or url query parameter."};       
        {not_authenticated, Reason} ->
          #pe_rest_response{status=401, content_type="text/plain", body=io_lib:format("Unable to authenticate: ~s", [Reason])}
      end;
    invalid ->
      #pe_rest_response{status=400, body="Invalid principal id format"}
  end.
    
handle_get_active_subscriptions_for_principal(Req, PrincipalIdInput) ->
  case pe_util:validate_input(normal, PrincipalIdInput) of
    valid ->
      case authenticate(Req, undefined, PrincipalIdInput, undefined) of
        {authenticated_ok, PrincipalId} ->
          case PrincipalIdInput == PrincipalId of
            true ->
              Subs = pe_sub_store:get_all_active_for_principal(PrincipalId),
              #pe_rest_response{content_type="application/json", body=mochijson2:encode(get_subscriptions_struct(Subs))};
            false ->
              #pe_rest_response{status=401, content_type="text/plain", body=io_lib:format("Principal Id from the Authorization token (~s) does not match the one provided in the URL (~s).",[PrincipalId, PrincipalIdInput])}
          end;        
        {error, authorization_token_not_provided} ->
          #pe_rest_response{status=401, content_type="text/plain", body="An authorization token must be provided by including the Authorization request header or url query parameter."};       
        {not_authenticated, Reason} ->
          #pe_rest_response{status=401, content_type="text/plain", body=io_lib:format("Unable to authenticate: ~s", [Reason])}
      end;
    invalid ->
      #pe_rest_response{status=400, body="Invalid principal id format"}
  end.

handle_get_subscription(Req, Id) ->  
  case pe_util:validate_input(normal, Id) of
    valid ->
      case authenticate(Req, undefined, Id, undefined) of
        {authenticated_ok, PrincipalId} ->
          case pe_sub_store:lookup(Id) of
            {found, Sub} ->
              case PrincipalId == pe_sub:get(principal_id, Sub) of
                true ->
                  #pe_rest_response{content_type="application/json", body=mochijson2:encode(get_subscription_struct(Sub))};
                false ->
                  #pe_rest_response{status=401, content_type="text/plain", body="Subscription " ++ Id ++ " is not owned by principal " ++ PrincipalId}
              end;
            _ ->
              #pe_rest_response{status=404, body=io_lib:format("Subscription '~s' not found",[Id])}
          end;
        {error, authorization_token_not_provided} ->
          #pe_rest_response{status=401, content_type="text/plain", body="An authorization token must be provided by including the Authorization request header or url query parameter."};       
        {not_authenticated, Reason} ->
          #pe_rest_response{status=401, content_type="text/plain", body=io_lib:format("Unable to authenticate: ~s", [Reason])}
      end;
    invalid ->
      #pe_rest_response{status=400, body="Invalid subscription id format"}
  end.


get_post_var(Key, Req) ->
  get_post_var(Key, Req, urlencoded).
get_post_var(Key, Req, urlencoded) ->
  proplists:get_value(Key, Req:parse_post());
get_post_var(Key, Req, multipart) ->
  proplists:get_value(Key, mochiweb_multipart:parse_form(Req)).

parse_context_tags(String) ->
  pe_tag_util:parse_tags_string(String).

handle_post_subscription__has_minimum_tags_or_message_type("",undefined) ->
  false;
handle_post_subscription__has_minimum_tags_or_message_type(undefined,"") ->
  false;
handle_post_subscription__has_minimum_tags_or_message_type("","") ->
  false;
handle_post_subscription__has_minimum_tags_or_message_type(undefined,undefined) ->
  false;
handle_post_subscription__has_minimum_tags_or_message_type(_Tags,_MessageType) ->
  true.

value_or_empty(undefined) ->
  "";
value_or_empty(Val) ->
  Val.
  
make_signature_data(sub, Client, ClientString, System, SubSystem, CallBackUrl, WsdlUri, RawTags, MessageType) ->
  lists:flatten([value_or_empty(Client), value_or_empty(ClientString), value_or_empty(System), value_or_empty(SubSystem), value_or_empty(CallBackUrl),value_or_empty(WsdlUri),value_or_empty(RawTags),value_or_empty(MessageType)]).
  
make_signature_data(msg, Client, ClientString, System, SubSystem, Realm, UnparsedTags, MessageType, PayloadContentType, Payload) ->
  Tmp = list_to_binary(lists:flatten([value_or_empty(Client), value_or_empty(ClientString), value_or_empty(System), value_or_empty(SubSystem), value_or_empty(Realm), value_or_empty(UnparsedTags), value_or_empty(MessageType), value_or_empty(PayloadContentType)])),
  <<Tmp/binary,Payload/binary>>.
  



handle_post_subscription(Req) ->
  CallBackUrl = get_post_var("CALLBACK-URL",Req),
  WsdlUri = get_post_var("WSDL-URI",Req),
  RawTags = get_post_var("TAGS", Req),
  MessageType = get_post_var("MESSAGE-TYPE",Req),
  Auth = get_post_var("AUTHORIZATION",Req),
  Delim = get_post_var("AUTHORIZATION-DELIMITER",Req),
  Client = get_post_var("CLIENT", Req),
  ClientString = get_post_var("CLIENT-STRING", Req),
  System = get_post_var("SYSTEM", Req),
  SubSystem = get_post_var("SUB-SYSTEM", Req),
  
  case pe_util:validate_all_inputs([
    {CallBackUrl, callback_url, url},
    {WsdlUri, wsdl_uri, url},
    {MessageType, message_type, normal},
    {Client, client, normal},
    {ClientString, client_string, normal},
    {System, system, normal},
    {SubSystem, sub_system, normal},
    {Auth, authorization, auth},
    {Delim, authorization_delimiter, auth_delimiter},
    {RawTags, tags, tags}
  ]) of
    {invalid, Desc} ->
      #pe_rest_response{status=400, body=io_lib:format("Invalid input format: ~p",[Desc])};
      
    valid ->  
      ParsedTags = parse_context_tags(RawTags),
      case ParsedTags of
        {ok, TmpTags} ->
          Tmp1 = pe_tag_util:merge_client(TmpTags, Client),
          Tmp2 = pe_tag_util:merge_client_string(Tmp1, ClientString),
          Tmp3 = pe_tag_util:merge_system(Tmp2, System),
          Tmp4 = pe_tag_util:merge_sub_system(Tmp3, SubSystem),
          Tags = pe_tag_util:merge_message_type(Tmp4, MessageType), 

      case handle_post_subscription__has_minimum_tags_or_message_type(Tags, MessageType) of
          false ->
            #pe_rest_response{status=400, body="Please supply at least one TAGS or the MESSAGE-TYPE when creating a new subscription.  For example, 'TAGS=System:Tempest,Client:OnlineU' or MESSAGE-TYPE=UserLogin"};
          true ->
            AuthPayload = make_signature_data(sub, Client, ClientString, System, SubSystem, CallBackUrl, WsdlUri, RawTags, MessageType),
            case authenticate(Req, Auth, AuthPayload, Delim) of
              {authenticated_ok, PrincipalId} ->
                case pe_sub_intake:accept(CallBackUrl, Tags, PrincipalId, WsdlUri) of
                  {ok,Subscription} ->
                    #pe_rest_response{content_type="application/json", body=mochijson2:encode(get_subscription_struct(Subscription))};
                  {invalid_request, {missing_tag, ?MESSAGE_TYPE_TAG_TYPE}} ->
                    #pe_rest_response{status=400, content_type="text/plain", body="The MESSAGE-TYPE is required for all subscriptions belonging to Principal " ++ PrincipalId};
                  {invalid_request, {missing_tag, Tag}} ->
                    #pe_rest_response{status=400, content_type="text/plain", body="The context tag '" ++ Tag ++ "' is required"};
                  {invalid_request, {conflicting_properties, Desc}} ->
                    #pe_rest_response{status=400, content_type="text/plain", body="Conflicting properties: " ++ Desc};
                  {invalid_request, {incomplete_properties, Desc}} ->
                    #pe_rest_response{status=400, content_type="text/plain", body="Incomplete properties: " ++ Desc};
                  {invalid_request, {out_of_mask_url, Error}} ->
                    #pe_rest_response{status=400, content_type="text/plain", body="Invalid callback url: " ++ Error};
                  {invalid_request, {duplicate_subscription, DupedSubId}} ->
                    #pe_rest_response{status=409, content_type="text/plain", body="Duplicate of subscription " ++ DupedSubId, headers=[{"X-Prospero-Subscription-Id", DupedSubId}]};
                  {invalid_request, {url_is_spam, {ErrorMsg, LogData}}} ->
                    pe_audit:log(LogData,"SUBSCRIPTION-REJECTED"),
                    #pe_rest_response{status=400, content_type="text/plain", body="Callback URL failed validation: " ++ ErrorMsg};
                  {error, unable_to_connect_to_broker} ->
                    #pe_rest_response{status=500, content_type="text/plain", body="Unable to intake subscription: There is a problem with the messaging infrastructure (unable_to_connect_to_broker), please contact your administrator."};
                  {error, Error} ->
                    #pe_rest_response{status=500, content_type="text/plain", body=io_lib:format("Unable to intake subscription: ~p",[Error])}
                end;
              {error, authorization_token_not_provided} ->
                #pe_rest_response{status=401, content_type="text/plain", body="An authorization token must be provided by including the AUTHORIZATION form field."};       
              {not_authenticated, Reason} ->
                #pe_rest_response{status=401, content_type="text/plain", body=io_lib:format("Unable to authenticate: ~s", [Reason])}
                   end
          end;
        {unrecognized_format, ShorthandString} ->
          #pe_rest_response{status=400, content_type="text/plain", body="The context tag '" ++ ShorthandString ++ "' is an invalid format"}
      end
  end.

get_publish_consistency_or_default(Req) ->
  case Req:get_primary_header_value("x-publish-consistency") of
    "sync" -> 
	case is_durable_messaging_enabled(Req) of
		true -> true;
		false -> unauthorized
	end;
    "async" -> false;
    _ -> pe_config:get(amqp, confirmed_publishing, ?DEFAULT_CONFIRMED_PUBLISHING)
  end.

handle_post_message(Req) ->
	case get_publish_consistency_or_default(Req) of
		unauthorized -> 
			#pe_rest_response{status=403, body="FORBIDDEN: Request to publish message with publish conistency is forbidden for this principal."};
		_Val -> 
			IsConfirmedPublishing = _Val,
			case Req:get_primary_header_value("content-type") of
				"application/x-www-form-urlencoded" ++ _ ->
					handle_post_message_urlencoded(Req, IsConfirmedPublishing);
				"multipart/form-data" ++ _ ->
					handle_post_message_multipart(Req, IsConfirmedPublishing);
				_ ->
					#pe_rest_response{status=400, body="Messages must be posted with either a query string style application/x-www-form-urlencoded format or mime encoded with multipart/form-data"}
			end				
	end.


validate_payload_content_type(Arg) -> pe_util:validate_payload_content_type(Arg).

intake_post(Req, Payload, UnparsedTags, MessageType, Auth, Delim, Client, ClientString, System, SubSystem, "", IsConfirmedPublishing) ->
  intake_post(Req, Payload, UnparsedTags, MessageType, Auth, Delim, Client, ClientString, System, SubSystem, undefined, IsConfirmedPublishing);

intake_post(Req, Payload, UnparsedTags, MessageType, Auth, Delim, Client, ClientString, System, SubSystem, undefined, IsConfirmedPublishing) ->
  intake_post(Req, Payload, UnparsedTags, MessageType, Auth, Delim, Client, ClientString, System, SubSystem, "*", IsConfirmedPublishing);

intake_post(Req, PayloadAndContentType, UnparsedTags, MessageType, Auth, Delim, Client, ClientString, System, SubSystem, Realm, IsConfirmedPublishing) ->
  case PayloadAndContentType of
    {_, undefined} ->
      #pe_rest_response{status=400, body="Please supply the PAYLOAD"};
    {_, ""} ->
      #pe_rest_response{status=400, body="Please supply the PAYLOAD"};
    {_, <<>>} ->
      #pe_rest_response{status=400, body="Please supply the PAYLOAD"};
    {"", _} ->
      #pe_rest_response{status=400, body="Please supply the PAYLOAD-CONTENT-TYPE"};
    {undefined, _} ->
      #pe_rest_response{status=400, body="Please supply the PAYLOAD-CONTENT-TYPE"};
    {<<>>, _} ->
      #pe_rest_response{status=400, body="Please supply the PAYLOAD-CONTENT-TYPE"};
    {PayloadContentType, Payload} ->
   
     ParsedTags = parse_context_tags(UnparsedTags),
     Tags = case ParsedTags of
       {ok, TmpTags} ->
         Tmp1 = pe_tag_util:merge_client(TmpTags, Client),
         Tmp2 = pe_tag_util:merge_client_string(Tmp1, ClientString),
         Tmp3 = pe_tag_util:merge_system(Tmp2, System),
         Tmp4 = pe_tag_util:merge_sub_system(Tmp3, SubSystem),
         Tmp5 = pe_tag_util:merge_message_type(Tmp4, MessageType),
         pe_tag_util:merge_message_type(Tmp5, Realm);
       {unrecognized_format, ShorthandString} ->
         #pe_rest_response{status=400, content_type="text/plain", body="The context tag '" ++ ShorthandString ++ "' is an invalid format"}
     end,
 
      case pe_util:validate_all_inputs([
        {PayloadContentType, payload_content_type, content_type},
        {MessageType, message_type, normal},
        {Client, client, normal},
        {ClientString, client_string, normal},
        {System, system, normal},
        {SubSystem, sub_system, normal},
        {Auth, authorization, auth},
        {Delim, authorization_delimiter, auth_delimiter},
        {Realm, realm, realm},
        {UnparsedTags, tags, tags}
      ]) of
        {invalid, Desc} ->
          #pe_rest_response{status=400, body=io_lib:format("Invalid input format: ~p",[Desc])};
        valid ->  
          case validate_payload_content_type(PayloadContentType) of
            valid ->
              case MessageType of
                undefined ->
                  #pe_rest_response{status=400, body="Please supply the MESSAGE-TYPE"};
                "" ->
                  #pe_rest_response{status=400, body="MESSAGE-TYPE cannot be an empty string"};
                _ ->
                  AuthData = make_signature_data(msg, Client, ClientString, System, SubSystem, Realm, UnparsedTags, MessageType, PayloadContentType, Payload),
                      
                  case authenticate(Req, Auth, AuthData, Delim) of
                    {authenticated_ok, PrincipalId} ->
                      %NOTE: This module can't support async publishing module because of the stupid {loop, Loop} construct that MochiWeb uses, so pe_msg_intake is hard-coded to use the sync model.
                          
                      try publish(Payload, MessageType, Tags, PrincipalId, Realm, PayloadContentType, IsConfirmedPublishing) of
                        Val -> Val
                      catch
                        _ErrorType:Error ->
                          error_logger:error_msg("Problem publishing message: ~p ~p~n", [_ErrorType, Error]),
                          #pe_rest_response{status=500, content_type="text/plain", body="An error occurred while publishing the message"}
                      end;
                    {error, authorization_token_not_provided} ->
                      #pe_rest_response{status=401, content_type="text/plain", body="An authorization token must be provided by including the AUTHORIZATION form field."};       
                    {not_authenticated, Reason} ->
                      #pe_rest_response{status=401, content_type="text/plain", body=io_lib:format("Unable to authenticate: ~s", [Reason])}
                  end
               end;
             invalid ->
               #pe_rest_response{status=400, body="Illegal PAYLOAD-CONTENT-TYPE: " ++ PayloadContentType}
         end
      end          
  end.
 
publish(Payload, MessageType, Tags, PrincipalId, Realm, PayloadContentType, IsConfirmedPublishing) ->
	case pe_msg_intake:accept(Payload, MessageType, Tags, PrincipalId, Realm, PayloadContentType, IsConfirmedPublishing) of
		{publish_confirm,MessageId} ->
			#pe_rest_response{content_type="application/json", body=io_lib:format("{\"message\": {\"id\": \"~s\"}}",[MessageId])};
		{publish_wait_timeout, _MessageId} ->
			% After receiving a publish_wait_timeout send to recieve loop to wait for pub/confirm response %
			handle_publish_confirm_response();
		{invalid_request, {missing_tag, Tag}} ->
			#pe_rest_response{status=400, content_type="text/plain", body="The context tag '" ++ Tag ++ "' is required"};
		{error, no_available_brokers} ->
			#pe_rest_response{status=500, content_type="text/plain", body="Unable to intake message: There is a problem with the messaging infrastructure (no_available_brokers), please contact your administrator."};
		{error, Error} ->
		#pe_rest_response{status=500, content_type="text/plain", body=io_lib:format("Unable to intake message: ~p",[Error])}
	end.

handle_post_message_urlencoded(Req, IsConfirmedPublishing) ->   
  PayloadContentType = get_post_var("PAYLOAD-CONTENT-TYPE",Req),
  Payload = bin(get_post_var("PAYLOAD",Req)),
    
  MessageType = get_post_var("MESSAGE-TYPE",Req),
  Tags = get_post_var("TAGS", Req),
  Auth = get_post_var("AUTHORIZATION", Req),
  Delim = get_post_var("AUTHORIZATION-DELIMITER", Req),
  Client = get_post_var("CLIENT", Req),
  ClientString = get_post_var("CLIENT-STRING", Req),
  System = get_post_var("SYSTEM", Req),
  SubSystem = get_post_var("SUB-SYSTEM", Req),
  Realm = get_post_var("REALM", Req),
  
  intake_post(Req, {PayloadContentType, Payload}, Tags, MessageType, Auth, Delim, Client, ClientString, System, SubSystem, Realm, IsConfirmedPublishing).
  

parse_payload_part(Payload) when is_list(Payload) ->
  bin(Payload);
parse_payload_part({_FileName, {_ContentType, _Encoding}, Payload}) ->
  bin(Payload).


handle_post_message_multipart(Req, IsConfirmedPublishing) ->
  Parsed = mochiweb_multipart:parse_form(Req),

  MessageType = proplists:get_value("MESSAGE-TYPE", Parsed),
  Tags = proplists:get_value("TAGS", Parsed),
  
  PayloadContentType = proplists:get_value("PAYLOAD-CONTENT-TYPE", Parsed),
  Payload = parse_payload_part(proplists:get_value("PAYLOAD", Parsed)),

  Auth = proplists:get_value("AUTHORIZATION", Parsed),
  Delim = proplists:get_value("AUTHORIZATION-DELIMITER", Parsed),
  Client = proplists:get_value("CLIENT", Parsed),
  ClientString = proplists:get_value("CLIENT-STRING", Parsed),
  System = proplists:get_value("SYSTEM", Parsed),
  SubSystem = proplists:get_value("SUB-SYSTEM", Parsed),
  Realm = proplists:get_value("REALM", Parsed),
   
  intake_post(Req, {PayloadContentType, Payload}, Tags, MessageType, Auth, Delim, Client, ClientString, System, SubSystem, Realm, IsConfirmedPublishing).


handle_delete_subscription(Req, Id) ->
  Auth = get_post_var("AUTHORIZATION",Req),
  Delim = get_post_var("AUTHORIZATION-DELIMITER",Req),


  case pe_util:validate_all_inputs([
    {Auth, authorization, auth},
    {Delim, authorization_delimiter, auth_delimiter},
    {Id, subscription_id, normal}
  ]) of
    {invalid, Desc} ->
      #pe_rest_response{status=400, body=io_lib:format("Invalid input format: ~p",[Desc])};
      
    valid ->  

      case authenticate(Req, Auth, Id, Delim) of
        {authenticated_ok, PrincipalId} ->
          case pe_sub_store:lookup(Id) of
            {found, Sub} ->
              case PrincipalId == pe_sub:get(principal_id, Sub) of
                true ->
                  {ok, _Sub2} = pe_sub_intake:delete(Sub),
                  #pe_rest_response{content_type="text/plain", body="CANCELLED SUBSCRIPTION " ++ Id};
                false ->
                  #pe_rest_response{status=401, content_type="text/plain", body="Subscription " ++ Id ++ " is not owned by principal " ++ PrincipalId}
              end;
            _ ->
              #pe_rest_response{status=404, body=io_lib:format("Subscription '~s' not found",[Id])}
          end;
        {error, authorization_token_not_provided} ->
          #pe_rest_response{status=401, content_type="text/plain", body="An authorization token must be provided by including the Authorization request header or url query parameter."};       
        {not_authenticated, Reason} ->
          #pe_rest_response{status=401, content_type="text/plain", body=io_lib:format("Unable to authenticate: ~s", [Reason])}
      end
  end.


get_status_struct({Node, {ok, {status,_NodeName, Version,NumMsg,NumDel,NumFailed,TimeStarted,Uptime,NumWatchedSubs, SubPids, PidsAwaitingBrokerReconnect, RestStatus}}}) ->
  {struct, [
    {node,
      {struct, [
        {name, Node},
        {version, Version},
        {start_time, bin(pe_time:format_8601(TimeStarted))},
        {uptime_seconds, Uptime},
        {num_messages_posted, NumMsg},
        {num_messages_delivered, NumDel},
        {num_failed_deliveries, NumFailed},
        {num_watched_subscriptions, NumWatchedSubs},
        {rest_status, RestStatus},
        {watched_subscriptions, get_subs_structs(SubPids)},
        {processes_awaiting_broker_reconnection, get_processes_awaiting_broker_reconnection_structs(PidsAwaitingBrokerReconnect)}
      ]}  
    }
  ]}.

get_processes_awaiting_broker_reconnection_structs(Items) ->
  lists:map(fun({_Pid, {Host, Description, WillTryAgainAt, NumFailedAttempts}}) -> {struct, [{proc, {struct, [{description,bin(Description)},{host, bin(Host)},{will_try_again_at,bin(pe_time:format_8601(WillTryAgainAt))},{num_failed_attempts, NumFailedAttempts}]}}]} end,Items).

get_subs_structs(SubPids) ->
  lists:map(fun({SubId, Pids}) -> {struct, [{proc, {struct, [{subscription_id, bin(SubId)},{num_pids, length(Pids)}]}}]} end,SubPids).
  
get_status_structs(Statuses) ->
  lists:map(fun(E) -> get_status_struct(E) end, lists:sort(fun({NodeA, _StatusA},{NodeB, _StatusB}) -> NodeA < NodeB end, Statuses)).

get_status_response_structs({Replies, BadNodes}) ->
  {struct, [
    {status,
      {struct, [
        {served_by, node()},
        {current_time, bin(pe_time:format_8601())},
        {up_nodes, get_status_structs(Replies)},
        {down_nodes, lists:map(fun(E) -> E end,BadNodes)}
      ]}
    }
  ]}.

handle_status(_Req) ->
  Response = pe_status:get_aggregated_cluster_status(),
   #pe_rest_response{content_type="application/json", body=mochijson2:encode(get_status_response_structs(Response))}.

handle_status_simple(_Req) ->
  case pe_config:get(rest,status,undefined) of
    serverin ->
      #pe_rest_response{body="OK"};
    serverout ->
      #pe_rest_response{status=503, body="SERVER-OUT"}
  end.
   
handle_create_signature(Req) ->
  Key = get_post_var("KEY",Req),
  Payload = get_post_var("PAYLOAD", Req),
  Principal = get_post_var("PRINCIPAL", Req),
  Delim = get_post_var("DELIMITER", Req),
  Date = pe_time:format_8601(),
    
  Sig = pe_util:create_signature(Payload, Key, Date),
  Result = pe_auth:make_token(Principal, Date, Sig, default_delimiter(Delim)),
  #pe_rest_response{content_type="application/json", body=io_lib:format("{\"token\": \"~s\"}",[Result])}.
  
% Receive loop for handling publish confirm responses from RabbitMQ ack % 
% The possible return values are as follows:				%
% 	publish_confirm: This means that RabbitMQ has successfully	%
% 			written the message to disk.			%
%	publish_reject: This response indicates that, for some reason	%
%			RabbitMQ was unable to confirm the publishing	%
%			and returned a basic.nack response.		%
handle_publish_confirm_response() ->
	receive 
		{publish_confirm, MessageId} ->
			#pe_rest_response{content_type="application/json", body=io_lib:format("{\"message\": {\"id\": \"~s\"}}",[MessageId])};
		{publish_reject, MessageId} ->
			error_logger:error_msg("pe_rest:handle_publish_confirm_response received a publish_reject for MsgId ~s~n", [MessageId]),
			#pe_rest_response{status=500, content_type="text/plain", body=io_lib:format("Publishing of Message message failed during confirm stage: ~s",[MessageId])};
		exit ->
			ok;
		_ -> 
			error_logger:error_msg("A non-standard response was received in pe_rest_publish_confirm_response. Sending server error to publisher.~n", []),
			#pe_rest_response{status=500, content_type="text/plain", body=io_lib:format("An unknown error occurred during the publishing of Message.",[])}
	after
		20000 ->
			#pe_rest_response{status=500, content_type="text/plain", body=io_lib:format("Unable to intake message. Reason: ~s",["Timeout expired"])}
	end.	

% This method determines if the publisher has the permissions necessary to request publish_confirms % 
is_durable_messaging_enabled(Req) ->
	Auth = get_post_var("AUTHORIZATION", Req),
	Delim = case get_post_var("AUTHORIZATION-DELIMITER", Req) of
			undefined -> "|";
			_Del -> _Del
		end,
	{ok, PrincipalId, _Date, _Signature} = pe_auth:parse_authorization(Auth, Delim),
	Principal = case pe_principal_store:lookup(PrincipalId) of
			{found, FoundPrincipal} -> FoundPrincipal;
			_ -> undefined
		    end,
	pe_principal:get(durable_messaging_enabled, Principal).
