% SubPub -  Copyright (c) 2013 Pearson.  All rights reserved.  
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%
%   you may not use this file except in compliance with the License.

-module(pe_admin_auth).
-export([ 
	load_key/1,
	shaHex/1,
	sign/2, 
	sign_file/1,
	verify/3,
	verify_file/2
]).

%
% Create a hex string of the SHA1 of the input string
%
shaHex (Message) ->
	SHA1 = crypto:sha(Message),
	hex:bin_to_hexstr(SHA1).

%
% sign a string
%
% This function computes the SHA1 of the ClearText, then encrypts that 
% value with an RSA secret key.  It converts the resulting bytes to hex
% and returns the string.
%
sign(ClearText, PrivateKey) ->
	SignatureBytes = crypto:sha(ClearText),
	CypherText = public_key:encrypt_private(SignatureBytes, PrivateKey),
	hex:bin_to_hexstr(CypherText).

%
% sign a string using the private key from the config value
%
% This function looks up the name of the private key file via pe_config:get.
% It then uses that key to sign the input string as per the sign/2 function.
%
sign_file(ClearText) ->
	PrivateKeyFile = pe_config:get(admin, private_key, undefined),
	PrivateKey = load_key(PrivateKeyFile),
	sign(ClearText, PrivateKey).

%
% verify a signature
%
% This function verifies that a specified password (the ClearText) matches
% the signature (CipherTextHex) by converting (CipherTextHex) to a binary,
% descrypting the resulting value with the provided public key, the comparing
% the computed SHA1 with that value.
%
verify (CipherTextHex, PublicKey, ClearText) ->
	Expected = crypto:sha(ClearText),

	CipherText = hex:hexstr_to_bin(CipherTextHex),
	Actual = public_key:decrypt_public(CipherText, PublicKey),

	Expected == Actual.

%
% verify a signature, using the public key from the config file
%
% This function is the same as verify/3, except that it gets the name 
% for the public key from the config file.  It then loads the key and 
% verifies the signature as normal.
%
verify_file(CipherTextHex, ClearText) ->
	PublicKeyFile = pe_config:get(admin, public_key, undefined),
	PublicKey = load_key(PublicKeyFile),
	verify (CipherTextHex, PublicKey, ClearText).

%
% extract a public or private key from a file
%
% The file is expected to be in PEM format.
%
load_key (FileName) ->
	{ ok, IOList } = file:read_file(FileName),
	Data = iolist_to_binary(IOList),
	[ KeyEntry ] = public_key:pem_decode(Data),
	public_key:pem_entry_decode(KeyEntry).

