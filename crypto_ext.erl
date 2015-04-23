-module(crypto_ext).
-export([block_encrypt/3, block_decrypt/3]).

-include_lib("eunit/include/eunit.hrl").

-on_load(init/0).

init() ->
    erlang:load_nif("./crypto_ext_nif", 0).

-spec block_decrypt(atom(), crypto:block_key(), binary()) -> iodata().
block_decrypt(_Type, _Key, _CipherText) ->
    "Not loaded".

-spec block_encrypt(atom(), crypto:block_key(), iodata()) -> binary().
block_encrypt(_Type, _Key, _PlainText) ->
    "Not loaded".

encode_decode_test() ->	
	PlainText = <<"Encryption test">>,
	Key = <<16#ab,16#cd,16#ef,16#01,16#23,16#45,16#67,16#89,
			16#bb,16#1b,16#5d,16#17,16#73,16#76,16#9d,16#a2>>,
	CipherText = crypto_ext:block_encrypt(aes_ecb128,Key,PlainText),
	PlainTextDecoded = crypto_ext:block_decrypt(aes_ecb128,Key,CipherText),

	?assert(PlainText =:= PlainTextDecoded).