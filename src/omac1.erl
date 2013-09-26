% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(omac1).

-export([generate_tag_aes_cbc_128/2]).

generate_tag_aes_cbc_128(Key, Data) ->
  Zeroes = <<0:128>>,
  IV = <<0:128>>,
  EncryptedZeroes = crypto:aes_cbc_128_encrypt(Key, IV, Zeroes),
  Lu = case most_significant_bit(EncryptedZeroes) of
    0 -> 
      shift_left(EncryptedZeroes);
    1 ->
      bxor_last_byte(shift_left(EncryptedZeroes))
  end,
  Lu2 = case most_significant_bit(Lu) of
    0 ->
      shift_left(Lu);
    1 ->
      bxor_last_byte(shift_left(Lu))
  end,
  Encrypted = cmac_aes_cbc_128(Key, Data, Lu, Lu2),
  Encrypted.
  
  
bxor_last_byte(<<Front:15/binary,Back:1/binary>>) ->
  TmpInt = as_integer(Back),
  Tmp = TmpInt bxor as_integer(<<135>>),
  TmpB = <<Tmp>>,
  Result = concat(Front,TmpB),
  Result.
  

   
cmac_aes_cbc_128(Key, <<>>, _Lu, Lu2) ->  
  IV = <<0:128>>,
  PaddedMessage = pad_binary_128(<<128>>),
  ToBeEncrypted = do_bxor( PaddedMessage, Lu2),
  Encrypted = crypto:aes_cbc_128_encrypt(Key, IV, ToBeEncrypted),
  Encrypted;

cmac_aes_cbc_128(Key, Data, Lu, Lu2) ->
  IV = <<0:128>>,
  Context = <<0:128>>,
  cmac_aes_cbc_128(Key, Data, IV, Context, Lu, Lu2).

cmac_aes_cbc_128(_Key, <<>>, _IV, Context, _Lu, _Lu2) ->
  Context;
  
cmac_aes_cbc_128(Key, <<Message:16/binary>>, IV, Context, Lu, _Lu2) ->
  ToBeEncrypted = do_bxor( do_bxor(Message, Context), Lu),
  Encrypted = crypto:aes_cbc_128_encrypt(Key, IV, ToBeEncrypted),
  Encrypted;

cmac_aes_cbc_128(Key, <<Message:16/binary, Remainder/binary>>, IV, Context, Lu, Lu2) ->
  ToBeEncrypted = do_bxor(Message, Context),
  Encrypted = crypto:aes_cbc_128_encrypt(Key, IV, ToBeEncrypted),
  cmac_aes_cbc_128(Key, Remainder, IV, Encrypted, Lu, Lu2);

cmac_aes_cbc_128(Key, <<Message/binary>>, IV, Context, _Lu, Lu2) ->
  PaddedMessage = pad_binary_128(<<Message/binary,128>>),
  ToBeEncrypted = do_bxor( do_bxor(PaddedMessage, Context), Lu2),
  Encrypted = crypto:aes_cbc_128_encrypt(Key, IV, ToBeEncrypted),
  Encrypted.
  
  
pad_binary_128(Data) when bit_size(Data) > 128 ->
  {Kept, _Lost} = split_binary(Data, size(Data) - 1),
  pad_binary_128(Kept);
pad_binary_128(Data) when bit_size(Data) < 128 ->
  pad_binary_128(<<Data/binary,0:1>>);
pad_binary_128(Data) ->
  Data.


most_significant_bit(<<Head:1/binary,_/binary>>) ->
  (as_integer(Head) band 255) bsr 7.


shift_left(ByteArray) ->
  shift_left(ByteArray,<<>>).
  
shift_left(<<>>, Shifted) ->
  Shifted;
 
shift_left(<<H:1/binary, H2:1/binary>>, <<Shifted/binary>>) ->
  Tmp = as_integer(H) bsl 1 + most_significant_bit(H2),
  Tmp2 = as_integer(H2) bsl 1,
  shift_left(<<>>, concat(Shifted, concat(<<Tmp>>, <<Tmp2>>)) );

shift_left(<<H:1/binary,H2:1/binary, T/binary>>, <<Shifted/binary>>) ->
  Tmp = as_integer(H) bsl 1 + most_significant_bit(H2),
  shift_left(concat(<<H2/binary>>, T), concat(Shifted, <<Tmp>>) ).
  
  
concat(B1, <<>>) ->
  <<B1/binary>>;
concat(<<>>, B2) ->
  <<B2/binary>>;
concat(B1, B2) ->
  <<B1/binary,B2/binary>>.
  
      
as_integer(<<I:1/binary>>) ->
  <<Ret/unsigned-integer>> = I,
  Ret.
  
  
do_bxor(B1, B2) when is_binary(B1), is_binary(B2) ->
  list_to_binary(do_bxor(binary_to_list(B1), binary_to_list(B2)));
  
do_bxor(L1, L2) when is_list(L1), is_list(L2) ->
  do_bxor(lists:reverse(L1), lists:reverse(L2), []).

do_bxor([], L2, Ret) ->
  lists:reverse(L2) ++ Ret;
do_bxor(L1, [], Ret) ->
  lists:reverse(L1) ++ Ret;
do_bxor([I1|Rest1], [I2|Rest2], Acc) ->
  do_bxor(Rest1, Rest2, [I1 bxor I2|Acc]).
