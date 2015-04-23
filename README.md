# crypto_ext
Provide an extension of crypto module to support AES 128 ECB cipher. This erlang module contains NIFs (Native Implemented Function). The encryption algorithm relies on the C openssl libraries, extracted in the folder `libopenssl/`. 

## Compilation

The directory contains a Makefile, that generates the .so and .beam files

	make

## Usage

**Encrypt binary**

	PlainText = <<"Encryption test">>,
	Key = <<16#ab,16#cd,16#ef,16#01,16#23,16#45,16#67,16#89,
			16#bb,16#1b,16#5d,16#17,16#73,16#76,16#9d,16#a2>>,
	CipherText = crypto_ext:block_encrypt(aes_ecb128,Key,PlainText).
	

**Decrypt binary**

	PlainTextDecoded = crypto_ext:block_decrypt(aes_ecb128,Key,CipherText).