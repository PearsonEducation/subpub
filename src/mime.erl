% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(mime).

-export([encode/1, mime_part/4]).

-include("include/mime.hrl").

mime_part(Type, Disposition, Encoding, Data) ->
  #mime_part{type=Type, disposition=Disposition, encoding=Encoding, data=Data}.
  

encode(Parts) ->
  Boundary = unique_boundary(),
  {mime, Boundary, encode_parts(Parts, Boundary)}.
  
encode_parts(Parts, Boundary) ->
  encode_parts(Parts, Boundary, []).
  
encode_parts([H|T], Boundary, Result) ->
  encode_parts(T, Boundary, Result ++ encode_part(H, Boundary));
  
encode_parts([], Boundary, Result) ->
  [Result, "--", Boundary, "--\r\n"].
  
  
encode_part(Part, _Boundary) when Part#mime_part.data == undefined ->
  "";
  
encode_part(Part, Boundary) ->
  ["--", Boundary, "\r\n",
  key_and_value_or_nothing("Content-Disposition",Part#mime_part.disposition),
  key_and_value_or_nothing("Content-Type",Part#mime_part.type),
  key_and_value_or_nothing("Content-Transfer-Encoding",Part#mime_part.encoding),
  "\r\n",
  encode_data(Part#mime_part.encoding, Part#mime_part.data, Part#mime_part.type),
  "\r\n"].
    
    
key_and_value_or_nothing(_Key, undefined) ->
  "";

key_and_value_or_nothing("Content-Transfer-Encoding", base64) ->
  "Content-Transfer-Encoding: base64\r\n";

key_and_value_or_nothing(Key, Value) ->
  Key ++ ": " ++ Value ++ "\r\n".

%encode_data(base64, Data, _Type) ->
%  base64:encode_to_string(Data);
  
encode_data(_Encoding, Data, _Type) ->
  Data.
  
  
unique_boundary() -> "++SUNFL0W3RS33D5++".

  




