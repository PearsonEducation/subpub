% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(sxml).
-compile(export_all).

-include_lib("xmerl/include/xmerl.hrl").

-include("include/sxml.hrl").

to_sxml(Xml) when is_record(Xml, xmlAttribute) ->
	{ Xml#xmlAttribute.name, Xml#xmlAttribute.value };

to_sxml(Xml) when is_record(Xml, xmlElement) ->
	case is_empty(Xml#xmlElement.content) of
		% element with no children
		true -> 
			#leaf {
				name = Xml#xmlElement.name,
				attrs = list_to_attrs(Xml#xmlElement.attributes)
			};

		_ -> 
			case is_text_list(Xml#xmlElement.content) of
				false ->
					#branch {
						name = Xml#xmlElement.name,
						attrs = list_to_attrs(Xml#xmlElement.attributes),
						children = list_to_sxml(Xml#xmlElement.content)
					};

				_ ->
					#leaf {
						name = Xml#xmlElement.name,
						attrs = list_to_attrs(Xml#xmlElement.attributes),
						value = to_value(Xml#xmlElement.content)
					}
			end
	end;

to_sxml(_) -> unrecognized.

is_text_list([]) -> true;
is_text_list([Head | Tail]) when is_record(Head, xmlText) ->
	is_text_list(Tail);
is_text_list([_ | _]) -> false.

to_value([]) -> "";
to_value([#xmlText{value = Value} | Rest]) ->
	Value ++ to_value(Rest).
	

list_to_attrs([]) -> [];
list_to_attrs([Head | Rest]) ->
	case to_sxml(Head) of 
		unrecognized -> list_to_attrs(Rest);
		Other -> [Other | list_to_attrs(Rest)]
	end.

list_to_sxml([]) -> [];
list_to_sxml([First | Rest]) ->
	case to_sxml(First) of
		unrecognized -> list_to_sxml(Rest);
		Other -> [Other | list_to_sxml(Rest)]
	end.

is_empty(undefined) -> true;
is_empty([]) -> true;
is_empty(_) -> false.


print_tree(Root) -> print_tree("", Root).

print_tree(Prefix, #leaf{name = Name, attrs = Attrs, value = Value}) 
	when Value == undefined ->

	io:format("~s~s", [Prefix, Name]),
	print_attributes(true, Attrs),
	io:format("~n");

print_tree(Prefix, #leaf{name = Name, attrs = Attrs, value = Value}) ->
	io:format("~s~s", [Prefix, Name]),
	print_attributes(true, Attrs),
	io:format(" = ~s~n", [Value]);

print_tree(Prefix, #branch{name = Name, attrs = Attrs, children = Children}) ->
	io:format("~s~s", [Prefix, Name]),
	print_attributes(true, Attrs),
	io:format("~n"),
	print_list(Prefix ++ "\t", Children).


print_list(_Prefix, []) -> nothing;

print_list(Prefix, [Head | Rest]) ->
	print_tree(Prefix, Head),
	print_list(Prefix, Rest).

print_attributes(true, []) -> nothing;

print_attributes(true, [{Name, Value}| Rest]) -> 
	io:format("{~s=~s", [Name, Value]),
	print_attributes(false, Rest);

print_attributes(false, []) ->
	io:format("}");

print_attributes(false, [{Name, Value} | Rest]) ->
	io:format(", ~s=~s", [Name, Value]),
	print_attributes(false, Rest).


get_attr(#leaf{attrs = Attrs}, Name) ->
	basic_get_attr(Name, Attrs);

get_attr(#branch{attrs = Attrs}, Name) ->
	basic_get_attr(Name, Attrs).


basic_get_attr(_Name, []) -> undefined;
basic_get_attr(Name, [ { Name, Value } | _Rest ]) -> Value;
basic_get_attr(Name, [ { _Name, _Value } | Rest ]) -> 
	basic_get_attr(Name, Rest).


get_child(_Name, #leaf{}) -> undefined;
get_child(Name, #branch{children = Children}) ->
	get_child_from_list(Name, Children).

get_child_from_list(_Name, []) -> undefined;
get_child_from_list(Name, [ First | Rest ]) ->
	case First of
		#leaf{name = Name} -> First;
		#leaf{name = _Other} -> 
			get_child_from_list(Name, Rest);

		#branch{name = Name} -> First;
		
		#branch{name = _Other} -> 
			get_child_from_list(Name, Rest)
	end.


get_child_value(Name, #branch{children = Children}) ->
	case get_child_from_list(Name, Children) of
		undefined -> undefined;
		Node -> 
			case Node of 
				#leaf{value = Value} -> Value;
				_ -> undefined
			end
	end;

get_child_value(_Name, _) -> undefined.

