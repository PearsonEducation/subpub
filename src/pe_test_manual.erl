% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_test_manual).

-export([post_multipart_message/3]).

create_mime_request(Payload, MessageType, Tags) ->
  {mime, Boundary, Body} = mime:encode([
    mime:mime_part(undefined,"form-data; name=\"MESSAGE-TYPE\"",undefined,MessageType),
    mime:mime_part(undefined,"form-data; name=\"TAGS\"",undefined,Tags),
    mime:mime_part("application/octet-stream","form-data; name=\"PAYLOAD\"; filename=\"payload\"",base64,Payload)
  ]),
  
  {mime, Boundary, Body}.
    
post_multipart_message(Payload, MessageType, Tags) ->
  {mime, Boundary, Body} = create_mime_request(Payload, MessageType, Tags),

  Headers = [{"Content-Type", "multipart/form-data; boundary=" ++ Boundary}],
  Options = [
    {content_type, "multipart/form-data; boundary=" ++ Boundary}
  ],

  ibrowse:send_req("http://localhost:4778/v1/message", Headers, post, Body, Options).
