CC=gcc
CFLAGS=-fPIC -shared
INCLUDES=-I /usr/lib/erlang/usr/include 

all: erlang

erlang: crypto_ext_nif
	erlc crypto_ext.erl

crypto_ext_nif:
	$(CC) $(CFLAGS) $(INCLUDES) libopenssl/aes_core.c libopenssl/aes_misc.c crypto_ext_nif.c  -o crypto_ext_nif.so

clean:
	rm *.so