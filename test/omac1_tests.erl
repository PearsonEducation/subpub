-module(omac1_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

generate_mac(HexKey, HexData, Expected) ->
  Result = hex:bin_to_hexstr(omac1:generate_tag_aes_cbc_128( hex:hexstr_to_bin(HexKey), hex:hexstr_to_bin(HexData) )),
  io:format("Expected ~s received ~s~n",[Expected, Result]),
  ?assert( Result == Expected ).

mac_test() ->
  generate_mac("2b7e151628aed2a6abf7158809cf4f3c", "6bc1bee22e409f96e93d7e117393172a", "070a16b46b4d4144f79bdd9dd04a287c"),
  generate_mac("2b7e151628aed2a6abf7158809cf4f3c", "6bc1bee22e409f96e93d7e117393172aae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411", "dfa66747de9ae63030ca32611497c827" ),
  generate_mac("2b7e151628aed2a6abf7158809cf4f3c", "6bc1bee22e409f96e93d7e117393172aae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52eff69f2445df4f9b17ad2b417be66c3710", "51f0bebf7e3b9d92fc49741779363cfe"),
  generate_mac("2b7e151628aed2a6abf7158809cf4f3c", "", "bb1d6929e95937287fa37d129b756746").

