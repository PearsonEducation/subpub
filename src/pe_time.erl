% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_time).

-export([
	current_milliseconds/0,
	current_timestamp/0, 
	format_8601/0, 
	format_8601/1, 
	now_milliseconds/0,
	parse_8601/1
]).

current_timestamp() ->
  calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(erlang:now())).
  
current_milliseconds() ->
  {Megasecs, Secs, Microsecs} = erlang:now(),
  ((Megasecs * 1000000) + Secs) * 1000 + (Microsecs / 1000).

% 
% This returns an integer number of milliseconds whereas current_milliseconds
% returns a floating point value
%
now_milliseconds() ->
	{MegaSeconds, Seconds, MicroSeconds} = now(),
	(MegaSeconds * 1000000000) + (Seconds * 1000) + (MicroSeconds div 1000).


format_8601() ->
  format_8601(current_timestamp()).
  
format_8601(undefined) ->
  "";
  
format_8601(Timestamp) ->
  {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:gregorian_seconds_to_datetime(Timestamp),
  lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2.10.0B:~2.10.0BZ",[Year, Month, Day, Hour, Min, Sec])).
  
get_offset_seconds(ZoneOffset) ->
  case ZoneOffset of
    [PlusMinus, TenHour, Hour, 58, TenMin, Min] ->  % 58 is a colon
      calculate_seconds(PlusMinus, TenHour, Hour, TenMin, Min);
    [PlusMinus, TenHour, Hour, TenMin, Min] ->
      calculate_seconds(PlusMinus, TenHour, Hour, TenMin, Min);
    [PlusMinus, TenHour, Hour] ->
      calculate_seconds(PlusMinus, TenHour, Hour, 0, 0);
    "Z" ->
      0
  end.

string_val(AsciiVal) ->
  [Result] = io_lib:format("~s",[[AsciiVal]]),
  Result.
  
calculate_seconds(PlusMinus, TenHour, Hour, TenMin, Min) ->
  Tmp = list_to_integer(string_val(Min)) * 60 + list_to_integer(string_val(TenMin)) * 600 + list_to_integer(string_val(Hour)) * 3600 + list_to_integer(string_val(TenHour)) * 36000,
  case string_val(PlusMinus) of
    "-" ->
      0 - Tmp;
    "+" ->
      Tmp
  end.
  
get_string({Start, Length}, String) ->
  string:sub_string(String, Start+1, Start+Length).

parse_8601(undefined) -> {ok, undefined};
parse_8601(DateString) ->
  case re:run(DateString,"^([0-9]{4})-([0-9]{2})-([0-9]{2})T([0-9]{2}):([0-9]{2}):([0-9]{2})(Z|[\-+]{1}[0-9]{2,4}|[\-+]{1}[0-9]{2}:[0-9]{2})$",[]) of
    {match, [_, YearMatch, MonthMatch, DayMatch, HoursMatch, MinutesMatch, SecondsMatch, ZoneOffsetMatch]} ->
      Year = get_string(YearMatch, DateString),
      Month = get_string(MonthMatch, DateString),
      Day = get_string(DayMatch, DateString),
      Hours = get_string(HoursMatch, DateString),
      Minutes = get_string(MinutesMatch, DateString),
      Seconds = get_string(SecondsMatch, DateString),
      ZoneOffset = get_string(ZoneOffsetMatch, DateString),
      
      DateTime = {{list_to_integer(Year),list_to_integer(Month),list_to_integer(Day)},{list_to_integer(Hours),list_to_integer(Minutes),list_to_integer(Seconds)}},
      
      BaseSeconds = calendar:datetime_to_gregorian_seconds(DateTime),
      OffsetSeconds = get_offset_seconds(ZoneOffset),
      {ok, BaseSeconds + OffsetSeconds};
    nomatch ->
      {error, not_a_valid_date_string}
  end.
    

