-module(pe_tag_util_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("../include/prospero.hrl").


get_id_test() ->
  ?assert(pe_tag_util:get_id(pe_tag_util:instance("1","2","3")) == "3").
  
get_type_test() ->
  ?assert(pe_tag_util:get_type(pe_tag_util:instance("1","2","3")) == "1").
  
get_value_test() ->
  ?assert(pe_tag_util:get_value(pe_tag_util:instance("1","2","3")) == "2").
  
instance_no_id_test() ->
  ?assert(pe_tag_util:instance("2", "4") == #pe_tag{id="2:4", type="2", value="4"}).

instance_with_id_test() ->
  ?assert(pe_tag_util:instance("2", "4", "3") == #pe_tag{id="3", type="2", value="4"}).
  
get_tag_ids_test() ->
  Records = [pe_tag_util:instance(2, 4, 1),pe_tag_util:instance(2, 4, 2), pe_tag_util:instance(2, 4, 3)],
  ?assert(pe_tag_util:get_tag_ids(Records) == [1,2,3]).

pretty_print_test() ->
  ?assert(pe_tag_util:pretty_print(pe_tag_util:instance("Type","value",3)) == "Type:value"),
  ?assert(pe_tag_util:pretty_print(pe_tag_util:instance(undefined,undefined,3)) == "undefined:undefined").
  
parse_test() ->
  ?assert(pe_tag_util:parse("Type:Value") == {ok, "Type", "Value"}).
  
make_tag_audit_log_tags_test() ->
  Records = [pe_tag_util:instance("Type1","Value1",3),pe_tag_util:instance("Type2","Value2",6), pe_tag_util:instance("Type3","Value3",9)],
  ?assert(pe_tag_util:make_tag_audit_log_tags(Records) == [{tag,"Type1:Value1"}, {tag,"Type2:Value2"}, {tag,"Type3:Value3"}]).
  
has_tag_test() ->
  Records = [pe_tag_util:instance("Type1","Value1",3),pe_tag_util:instance("Type2","Value2",6), pe_tag_util:instance("Type3","Value3",9)],
  ?assert(pe_tag_util:has_tag("Nonexistent",Records) =:= false),
  ?assert(pe_tag_util:has_tag("Type1",Records) =:= true).
  
has_all_required_tags_for_message_test() ->
  Records = [
    pe_tag_util:instance(?CLIENT_TAG_TYPE,"Value1",3),
    pe_tag_util:instance(?CLIENT_STRING_TAG_TYPE,"Value1",3),
    pe_tag_util:instance(?MESSAGE_TYPE_TAG_TYPE,"Value1",3),
    pe_tag_util:instance(?SYSTEM_TAG_TYPE,"Value1",3),
    pe_tag_util:instance(?SUB_SYSTEM_TAG_TYPE,"Value1",3)
  ],
  ?assert(pe_tag_util:has_all_required_tags_for_message(Records) =:= ok).
  
has_all_required_tags_for_message_missing_one_test() ->
  Records = [
    pe_tag_util:instance(?CLIENT_TAG_TYPE,"Value1",3),
    pe_tag_util:instance(?CLIENT_STRING_TAG_TYPE,"Value1",3),
    pe_tag_util:instance(?SYSTEM_TAG_TYPE,"Value1",3),
    pe_tag_util:instance(?SUB_SYSTEM_TAG_TYPE,"Value1",3)
  ],
  ?assert(pe_tag_util:has_all_required_tags_for_message(Records) =:= {missing, ?MESSAGE_TYPE_TAG_TYPE}).
  
merge_message_type_test() ->
  ?assertEqual(pe_tag_util:merge_message_type(["One:Two"],"Woop"), ["One:Two",{"MessageType","Woop"}]).

merge_message_type_empty_string_test() ->
  ?assertEqual(pe_tag_util:merge_message_type(["One:Two"],""), ["One:Two"]).

merge_message_type_undefined_test() ->
  ?assertEqual(pe_tag_util:merge_message_type(["One:Two"],undefined), ["One:Two"]).

