% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_admin).

-export([start_link/0]).

%
% A module for handling admin tasks
%
% This module provides REST functionality to do things like create new
% principals or delete subscriptions.
%
% SECURITY
%
% The security of this module is different from the rest of the system.  When a
% request comes in, the server looks for two cookies: "token" and "signature".
% If either is undefined, then the server rejects the request.
%
% The token cookie has the form:
%
%     random|time_issued|user|user type
%
% "random" is simply a random string of numbers.  "time_issued" is the number
% of milliseconds since the epoch that the token was created.  "user" is a name
% that uniquely identifies the user.  "user type" is a string that identifies
% what abilities the user has.  Recognized values are "super", "write" and
% "read".  The super and write users can use any service in the system.  read
% users can read data but cannot write it.
%
% The signature cookie is a hex string that is the encrypted digest of the
% token.  The digest is computed with SHA1.  The encryption is performed 
% using RSA with the private key given by the private_key parameter.
%
% To authenticate a request, the system takes the token and computes a SHA1
% digest.  Next it tries to decrypt the signature using the public key 
% defined by the public_key configuration property.  Next, it compares 
% the SHA1 of the token with the decrypted value of the signature.  If they
% match, then the request may be valid, otherwise, the system rejects the 
% request.
%
% Assuming the computed signature matches the provided value, the system 
% compares the issued timestamp from the token with the current time.  If 
% the issued timestamp is within +/- the token_threshold value from the 
% configuration file, then the request is accepted, otherwise it is rejected.
%
% CONFIGURATION FILE
%
% All of the configuration values that the module uses are in the "admin" 
% section of the configuration file.  The config values and their meanings 
% are:
% 
%     file_path --- the path, relative to where the program is started, 
%     that the program will look for files that it serves.  Note that this
%     is only for URLs relative to "/file".  
%
%	  Thus if the value for this parameter is "www" and the request URL is
%	  "/file/foo.html" then the program will look for the file "www/foo.html"
%
%     passwd --- the file, relative to where the system is started, where 
%     the "password file" can be found.  
%
%     port --- the port that the system listens to for admin requests.  This
%     defaults to 8080 if no other value is given.
%
%     public_key --- the file containing the public key, in PEM format, 
%     that the system should use when trying to authenticate a user.
%
%     private_key --- if the system is acting as its own authorization server,
%     then this value is the file that contains the private key in PEM 
%     format that should be used to create signatures.
%
%     token_threshold --- the window of time that a token is considered 
%     valid.
% 
% PASSWD FILE
%
% The module uses a password file to help manage security.  The file contains
% one line for each user in the system.  Each line has the format:
%
%     <name>:<description>:<digest>
%
% Where "name" should be a string that uniquely identifies the user in 
% the system.  "description" is an arbitrary text description of the user.
% "digest" is a SHA1 digest of the user's password.
%
% If the system is also acting as the authentication server, then entries are
% used to authenticate users.
%
% SERVICES
%
% The following services are available via this module:
%
% /subscriptions --- return a JSON list of all the subscriptions in the system.
%     All users can access this service.
%
% /subscription --- create a new subscription in the system.  The service expects
%     a JSON subscription in the body of the request.  Only super and write users
%     can use this service.
%
% /subscription/<ID>, HTTP GET --- get information about a subscription.  The
%     service returns a JSON subscription in the body of the response.  All users
%     can access this service.
%
% /subscription/<ID>, HTTP DELETE --- remove an existing subscription from the
%     system.  Only super and write users can access this service.
%
% /principals --- return a JSON list of all the principals in the system.  All 
%     users can access this service.
%
% /principal --- create a new principal in the system.  The service expects a
%     JSON principal in the body of the request.  Only super and write users 
%     can access this service.
%
% /principal/<ID>, HTTP GET --- get information about a principal.  The service
%     returns a JSON principal in the body of the response.  All users can 
%     access this service.
%
% /principal/<ID>, HTTP DELETE --- remove a principal from the system.  Note
%     that this will also remove all the subscriptions associated with the 
%     principal.  Only super and write users can access this service.
%
% /principal/<ID>/activate --- reactivate a previously deactivated principal.
%     Only super and write users can use this service.
%
% /principal/<ID>/deactivate --- deactivate a principal.  Only super and write
%     users can access this service.
%
% /registry/keys --- list the registry keys 
%
% /registry/contents --- list contents of the registry
%
% JSON
% 
% users
%
% {
%     "name": string,
%     "userType" : string,
%     "description": string,
%     "digest": string,
%     "password": string
% }
%
% The name should be unique among all the users in the system.  The userType
% should be one of "super", "write" or "read".  The digest should be a hex
% encoded string that is the SHA1 of the user's password.  The password field is
% not always present.  It it is present, it contains the ASCII string that is the
% password for the user.
% 
%
% principal
%
% {
%     "id": string,
%     "friendly_name": string,
%     "require_message_type": boolean,
%     "realm": string,
%     "delivery_url_mask": string,
%     "secret": string
%     "enforced_tags": [
%         {
%             "id": string,
%             "type": string,
%             "value": string
%         },
%         ...
%     ]
% }
%
% subscription
%
% {
%     "principal_id": string,
%     "callback_url": string,
%     "tags": [
%			tag_string, tag_string, ...
%	  ]
% }
%
% The tags are expected to each have the form:
%
%     <type>:<value>
%
% For example
% 
%     client:piazza
%

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

	TableId = ets:new(startTime, [named_table, public]),
	ets:insert(TableId, {startTime, get_epoch_msec()}),

	Port = pe_config:get(admin,port,4779),

	{ok, Pid} = case mochiweb_http:start([
		{loop, Loop},
		{name, ?MODULE},
		{port, Port}
	]) of
		{ok, MochiPid} -> {ok, MochiPid}
	end,
	error_logger:info_msg("Started mochiweb with pid ~p on port ~p~n",[Pid, Port]),
	{ok, Pid}.


get_epoch_msec() ->
	{ MegaSec, Sec, MicroSec } = now(),
	round((MegaSec * 1000000000) + (Sec * 1000) + (MicroSec/1000)).



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
  log_invalid_request(Status, Req, Body),
  NewBody = obfuscate_error_message(Status, Body, OverrideSecureMessage),
  Headers2 = lists:append(Headers, [{"Content-Type",ContentType},{"Server",pe_config:get(rest,server_nickname,node())}]),         
  Req:respond({Status,add_force_close_header(Headers2),NewBody}).
  
%
% check that a request is valid
%
% See the discussion about security for details on how this is done.
%
% If the request is valid, the function returns true.  If there is 
% a problem, then the function returns { error, <message> }.
%
validate_signature(Request) ->
	case Request:get_cookie_value("token") of
		undefined -> { error, "Missing cookie: token" };
		Token ->
			case Request:get_cookie_value("signature") of
				undefined -> { error, "Missing cookie: signature" };
				Signature ->
					KeyFile = pe_config:get(admin, public_key, undefined),
					Key = pe_admin_auth:load_key(KeyFile),
					case pe_admin_auth:verify(Signature, Key, Token) of
						false -> { error, "Signature failed to validate" };
						true -> true
					end
			end
	end.

%
% Check the token timestamp to determine if it falls within the window 
%
% See the discussion about security for details on how this works.
%
% The function returns valid if the token timestamp is within limits for
% the server.  If the token is outside of the allowed window, then the
% function returns { error, <Message> }.
%
% Note that the function assumes that the "token" cookie is defined in  
% the request.
% 
check_timestamp (Request) ->
	Token = Request:get_cookie_value("token"),
	List = re:split(Token, "\\|"),
	StrTimestamp = binary_to_list(lists:nth(2, List)),
	Timestamp = list_to_integer(StrTimestamp),

	TokenWindow = pe_config:get(admin, token_threshold, 1200000),

	NowMilliSeconds = pe_time:now_milliseconds(),
	DeltaMilliSeconds = NowMilliSeconds - Timestamp,

	case (DeltaMilliSeconds > (-1 * TokenWindow)) and (DeltaMilliSeconds < TokenWindow) of
		true -> valid;
		false -> { error, "Token has expired" }
	end.

%
% Check if a request is valid
%
% This function checks that the signature of a request is valid and 
% that the token falls within the window of time during which it is
% valid on this server.
%
% The function returns valid if the request is valid, and 
% { error, <Message> } otherwise.
%
authenticate(Request) ->
	case validate_signature(Request) of
		{ error, StrReason } -> { error, StrReason };
		true ->
			case check_timestamp (Request) of
				{ error, StrReason } -> { error, StrReason };
				valid -> true
			end
	end.

%
% Process a request
%
% The function will process requests, exempting some requests from 
% authentication.  The exempt requests are "/file/login.html" and 
% "/login".  All others are checked.
%
handle_request(Req, PathPrefix) ->
	case authenticate(Req) of
		true -> do_handle_request(Req, PathPrefix);
		{ error, StrReason } -> 
			send_response(Req, #pe_rest_response{status=401, body=StrReason})
	end.

%
% Process an authenticated request
%
% See the description of services for details.
%
% The function assumes that the request has already been authenticated 
% before the call is made.
%
do_handle_request(Req, PathPrefix) ->
	case string:tokens(Req:get(path),"/") of
		[PathPrefix, "subscriptions"] ->
			case Req:get(method) of
				'GET' ->
					send_response(Req, handle_list_subscriptions());
				_ ->
					send_response(Req, #pe_rest_response{status=405, body="Method Not Allowed"})
			end;

		[PathPrefix, "subscription"] ->
			case Req:get(method) of
				'POST' ->
					case authorize(Req, [ "super", "write" ]) of
						false ->
							send_response(Req, #pe_rest_response{status=401, body="User is not authorized to use operation"});

						_ ->
							send_response(Req, handle_create_subscription(Req))
					end;

				_ ->
					send_response(Req, #pe_rest_response{status=405, body="Method Not Allowed"})
			end;

		[PathPrefix, "subscription", Id] ->
			case Req:get(method) of
				'GET' ->
					send_response(Req, handle_get_subscription(Req, Id));

				'DELETE' ->
					case authorize(Req, ["super", "write"]) of
						false ->
							send_response(Req, #pe_rest_response{status=401, body="User is not authorized to delete subscriptions"});

						_ ->
							send_response(Req, handle_delete_subscription(Req, Id))
					end;

				_ ->
					send_response(Req, #pe_rest_response{status=405, body="Method Not Allowed"})
			end;

		[PathPrefix, "principals"] ->
			case Req:get(method) of
				'GET' ->
					send_response(Req, handle_list_principals());
				_ ->
					send_response(Req, #pe_rest_response{status=405, body="Method Not Allowed"})
			end;

		[PathPrefix, "principal"] ->
			case Req:get(method) of
				'POST' ->
					case authorize(Req, ["super", "write"]) of
						false ->
							send_response(Req, #pe_rest_response{status=401, body="User is not authorized to create principals"});
						_ ->
							send_response(Req, handle_create_principal(Req))
					end;
				_ ->
					send_response(Req, #pe_rest_response{status=405, body="Method Not Allowed"})
			end;

		[PathPrefix, "principal", Id] ->
			case Req:get(method) of
				'GET' ->
					send_response(Req, handle_get_principal(Req, Id));

				'DELETE' ->
					case authorize(Req, ["super", "write"]) of
						false ->
							send_response(Req, #pe_rest_response{status=401, body="User is not authorized to delete principals"});
						_ ->
							send_response(Req, handle_delete_principal(Id))
					end;

				_ ->
					send_response(Req, #pe_rest_response{status=405, body="Method Not Allowed"})
			end;

		[PathPrefix, "principal", Id, "deactivate"] ->
			send_response(Req, handle_deactivate_principal(Id));

		[PathPrefix, "principal", Id, "activate"] ->
			send_response(Req, handle_activate_principal(Id));

		[PathPrefix, "registry", "contents" ] ->
			case Req:get(method) of
				'GET' ->
					send_response(Req, handle_registry_contents());

				_ ->
					send_response(Req, #pe_rest_response{status=405, body="Method Not Allowed"})
			end;

		["file", "create_principal.html" ] ->
			handle_file_request (Req, "/file");

		["file", "create_subscription.html" ] ->
			handle_file_request (Req, "/file");

		["file", "principals.html" ] ->
			handle_file_request (Req, "/file");

		["file", "prospero.html" ] ->
			handle_file_request (Req, "/file");

		["file", "subscriptions.html" ] ->
			handle_file_request (Req, "/file");

		["file", "view_principal.html" ] ->
			handle_file_request (Req, "/file");

		["file", "view_subscription.html" ] ->
			handle_file_request (Req, "/file");
		_ ->
			send_response(Req, #pe_rest_response{status=404,body="Unknown resource pattern"})
	end.

% 
% Delete a principal from the system
%
% This method will also remove any subscriptions associated with 
% the principal before removing it.
%
handle_delete_principal (Id) ->
	{ok, _} = pe_principal_intake:deactivate({id, Id}),
	pe_principal_store:delete(Id),
	#pe_rest_response{status=200}.

%
% convert tags to the format that pe_principal_intake:accept wants
%
% This converts a list of pe_tag records to a list of tuples of the form
% { tag_type, tag_value }
%
convert_to_accept_format ([]) -> [];
convert_to_accept_format ([Head|Tail]) ->
	Result = { Head#pe_tag.type, Head#pe_tag.value },
	[ Result | convert_to_accept_format(Tail) ].

% 
% create a principal in the system
%
% The function accepts a JSON string containing the new principal and 
% creates a corresponding entry in the principals database.
%
handle_create_principal(Request) ->
	Body = Request:recv_body(),
	Principal = pe_json:decode_principal(Body),
	ConvertedTags = convert_to_accept_format (Principal#pe_principal.enforced_tag_ids),
	pe_principal_intake:accept (
		Principal#pe_principal.id,
		Principal#pe_principal.friendly_name,
		ConvertedTags,
		Principal#pe_principal.is_require_message_type_with_new_subs,
		Principal#pe_principal.secret,
		Principal#pe_principal.realm,
		Principal#pe_principal.delivery_url_mask
	),
	#pe_rest_response{status=200}.

%
% activate a principal
%
% This amounts to setting the date_deactivated field to undefined 
% for the principal in question.  
%
% If the principal is not found, the function sends a 404 response.
%
handle_activate_principal(Id) ->
	case pe_principal_store:lookup(Id) of
		none ->
			#pe_rest_response { status = 404 };
		{ found, Principal } ->
			pe_principal_intake:activate(Principal),
			#pe_rest_response { status = 200 }
	end.

%
% deactivate a principal
%
% This amounts to setting the date_deactivated field to a value.
%
% If the principal is not found, the function sends a 404 response.
%
handle_deactivate_principal(Id) ->
	case pe_principal_store:lookup(Id) of
		none ->
			#pe_rest_response { status = 404 };
		{ found, Principal } ->
			pe_principal_intake:deactivate(Principal),
			#pe_rest_response { status = 200 }
	end.

%
% load a subscription
%
% The function sends a 404 if the subscription cannot be found.
% 
handle_get_subscription(_Request, Id) ->
	case pe_sub_store:lookup(Id) of
		none ->
			#pe_rest_response { status = 404 };

		{found, Subscription} ->
			#pe_rest_response {
				status = 200,
				content_type="application/json",
				body=mochijson2:encode(pe_json:encode_subscription(Subscription))
			}
	end.

%
% create a new subscription
%
% The function expects the body of the request to contain a JSON
% encoded subscription
%
handle_create_subscription(Request) ->
	Body = Request:recv_body(),
	Subscription = pe_json:decode_subscription(Body),

	try validate_subscription(Subscription) of
		valid ->
			pe_sub_store:create_new(Subscription),
			#pe_rest_response { status = 200 };

		{invalid, Description} ->
			#pe_rest_response { status = 400, body = "invalid tag: " ++ atom_to_list(Description) }
	catch 
		(unrecognized_format) ->
			#pe_rest_response { status = 400, body = "unrecognized format" }
	end.

%
% check a new subscription for correct format
%
% This function returns valid if the subscription conforms to the 
% format requirements and invalid otherwise.  The function can also throw
% an exception if the tags use an unrecognized format.
%
validate_subscription (Subscription) ->
	TagsList = pe_tag_util:parse_tag_ids(Subscription#pe_sub.tag_ids),
	CallBackUrl 	= Subscription#pe_sub.callback_url,
	System 			= extract_tag(TagsList, "System"),
	SubSystem 		= extract_tag(TagsList, "Subsystem"),
	MessageType 	= extract_tag(TagsList, "MessageType"),
	Client 			= extract_tag(TagsList, "Client"),
	ClientString 	= extract_tag(TagsList, "ClientString"),
	RawTags 		= extract_raw_tags(TagsList),

	pe_util:validate_all_inputs([
		{CallBackUrl, 	callback_url, 	url},
		{MessageType, 	message_type, 	normal},
		{Client, 		client, 		normal},
		{ClientString, 	client_string, 	normal},
		{System, 		system, 		normal},
		{SubSystem, 	sub_system, 	normal},
		{RawTags, 		tags, 			tags}
	]).

%
% return the value of a tag, give a list of tags
%
% This function searches through a list of tags for a particular 
% type and returns the value of the first matching tag.
%
extract_tag([], _) -> "";
extract_tag([Tag| Tail], Type) ->
	case Tag#pe_tag.type == Type of
		true -> Tag#pe_tag.value;
		_ -> extract_tag(Tail, Type)
	end.

%
% return a list of tags IDs that are not part of the "standard" set.
%
% The "standard" set of tags are System, SubSystem, MessageType,
% Client and ClientString.  If a tag is *not* one of these then 
% the tag id is returned.
%
extract_raw_tags(List) ->
	extract_raw_tags(List, "").

%
% Return a list of tag IDs given a list of pe_tag records, and 
% a string containing the tag list.
%
extract_raw_tags([], Result) -> Result;
extract_raw_tags([Tag | Tail], Result) ->
	case Tag#pe_tag.type of 
		"System" 		-> extract_raw_tags(Tail, Result);
		"SubSystem" 	-> extract_raw_tags(Tail, Result);
		"MessageType" 	-> extract_raw_tags(Tail, Result);
		"Client" 		-> extract_raw_tags(Tail, Result);
		"ClientString" 	-> extract_raw_tags(Tail, Result);
		_ 				-> 
			Result2 = append_tag(Tag, Result),
			extract_raw_tags(Tail, Result2)
	end.

%
% append a tag ID to a string of tag IDs.
%
% The string should simply contain the tag ID if this is the first
% tag in the string, otherwise, it should return a list of the form
% <tag list>,<tag id>
%
append_tag(Tag, "") -> Tag#pe_tag.id;
append_tag(Tag, Result) ->
	Result ++ "," ++ Tag#pe_tag.id.

%
% delete a subscription from the system
%
% The function does not check to see if the subscription exists before
% deleting it
%
handle_delete_subscription(_Request, Id) ->
	pe_sub_intake:delete(Id),
	#pe_rest_response { status = 200 }.

%
% get a list of all the subscriptions in the system
%
% The list is a JSON encoded subscription list.
%
handle_list_subscriptions() ->
	Subscriptions = pe_sub_store:get_all_subscriptions(),
	#pe_rest_response {
		status = 200,
		content_type="application/json",
		body=mochijson2:encode(pe_json:encode_subscription_list(Subscriptions))
	}.

%
% serve a file on the server
%
% This function tries to lookup a specified file in the file system and return 
% its contents in the response.  
%
% The function sets the content type to "text/html" regardless of the file 
% contents.
% 
handle_file_request (Request, Prefix) ->
	LocalPath = pe_config:get(admin, file_path, "www"),
	FullPath = Request:get(path),

	case string:str(FullPath, Prefix) of
		0 -> SubString = FullPath;
		Index ->
			SubString = string:substr(FullPath, Index + length(Prefix))
	end,

	RevisedPath = LocalPath ++ SubString,
	{ok, Contents} = file:read_file(RevisedPath),
	send_response (Request, #pe_rest_response{ status=200, body = Contents, content_type="text/html"}).

%
% return all the principals in the system
%
% This function returns a JSON list containing all the principals 
% in the system.
%
handle_list_principals () ->
	Principals = pe_principal_store:get_all_principals(),
	Structs = pe_json:encode_principal_list(Principals),
	#pe_rest_response {
		status = 200,
		content_type="application/json",
		body=mochijson2:encode(Structs)
	}.

%
% return a specific principal
%
% The function tries to find the specified principal in the system.  If it cannot, it 
% sends a 404 to the client.
%
handle_get_principal (Request, Id) ->
	case pe_principal_store:lookup(Id) of
		none -> 
			send_response(Request, #pe_rest_response{ status=404, body= "Not found " ++ Id });
		{ found, Principal } ->
			#pe_rest_response {
				status = 200,
				content_type = "application/json",
				body = mochijson2:encode(pe_json:encode_principal(Principal))
			}
	end.

authorize(Request, TypeList) -> 
	UserType = get_user_type(Request),
	user_is_type(UserType, TypeList).

user_is_type(_User, []) -> false;
user_is_type(User, [UserType | Rest]) ->
	case User == UserType of
		true -> true;
		_ -> user_is_type(User, Rest)
	end.

get_user_type(Request) ->
	Token = Request:get_cookie_value("token"),
	[ _Random, _Timestamp, _User, UserType ] = re:split(Token, "\\|"),
	binary_to_list(UserType).

%
% The function returns a pe_rest_response that contains the contents 
% of the registry.  
% 
handle_registry_contents() ->
	Contents = pe_msg_intake:get_registry_contents(),
	[{startTime, StartTime}] = ets:lookup(startTime, startTime),
	UpTime = get_epoch_msec() - StartTime,
	#pe_rest_response {
		status = 200,
		content_type = "application/json",
		body = mochijson2:encode(pe_json:encode_registry(Contents,StartTime,UpTime))
	}.

