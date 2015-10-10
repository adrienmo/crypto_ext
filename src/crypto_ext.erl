-module(crypto_ext).
-export([block_encrypt/3, block_decrypt/3]).
-on_load(init/0).

-include_lib("eunit/include/eunit.hrl").

-define(APPNAME, crypto_ext).
-define(LIBNAME, crypto_ext).

init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).

-spec block_decrypt(atom(), crypto:block_key(), binary()) -> iodata().
block_decrypt(_Type, _Key, _CipherText) ->
    not_loaded(?LINE).

-spec block_encrypt(atom(), crypto:block_key(), iodata()) -> binary().
block_encrypt(_Type, _Key, _PlainText) ->
    not_loaded(?LINE).

encode_decode_test() ->
	PlainText = <<"Encryption test">>,
	Key = <<16#ab,16#cd,16#ef,16#01,16#23,16#45,16#67,16#89,
			16#bb,16#1b,16#5d,16#17,16#73,16#76,16#9d,16#a2>>,
	CipherText = crypto_ext:block_encrypt(aes_ecb128,Key,PlainText),
	PlainTextDecoded = crypto_ext:block_decrypt(aes_ecb128,Key,CipherText),

	?assert(PlainText =:= PlainTextDecoded).
