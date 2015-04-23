#include "erl_nif.h"

#include <memory.h>
#include <string.h>

#include "libopenssl/aes.h"

static ERL_NIF_TERM block_encrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary plain_text, out, key, type;
  unsigned char* padded_plain_text;
  char type_str[16] = "";

  //ARG1 : Type (atom(), only aes_ecb128 is supported)
  if(!enif_get_atom(env, argv[0], type_str, 16, ERL_NIF_LATIN1)){
    return enif_make_badarg(env);
  }else if(strcmp(type_str,"aes_ecb128")){
    return enif_make_badarg(env);
  }

  //ARG2 : Key (binary(), must be 16 bits)
  if(!enif_inspect_binary(env, argv[1], &key)) {
    return enif_make_badarg(env);
  }else if(key.size != 16) {
    return enif_make_badarg(env);
  }

  //ARG3 : PlainText (binary())
  if(!enif_inspect_binary(env, argv[2], &plain_text)) {
    return enif_make_badarg(env);
  }

  //Pad input with PKCS5Padding
  unsigned char padding =(unsigned char) (16 - plain_text.size % 16);
  padded_plain_text = (unsigned char*)malloc(sizeof(unsigned char) * plain_text.size + padding);
  strncpy(padded_plain_text, (unsigned char*)(plain_text.data), plain_text.size);
  int j = 0;
  for(j = 0; j < padding; j++) {
    padded_plain_text[plain_text.size + j] = padding;
  }

  //Allocate binary for the encrypted binary
  if(!enif_alloc_binary(plain_text.size + padding, &out)) {
    free(padded_plain_text);
    return enif_make_badarg(env);
  }

  //Set up encrypt_key
  struct aes_key_st* encrypt_key = (struct aes_key_st*)malloc(sizeof(AES_KEY));
  memset(encrypt_key, 0, sizeof(AES_KEY));
  AES_set_encrypt_key((unsigned char*)(key.data), 128, encrypt_key);

  //Perform encoding for each block
  int i = 0;
  for(i = 0; i < plain_text.size + padding; i += 16) {
    AES_encrypt((unsigned char*)&padded_plain_text[i], (unsigned char*)&out.data[i], encrypt_key);
  }

  free(padded_plain_text);
  free(encrypt_key);
  return enif_make_binary(env, &out);
}

static ERL_NIF_TERM block_decrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary cipher_text, out, key, type;
  char type_str[16] = "";

  //ARG1 : Type (atom(), only aes_ecb128 is supported)
  if(!enif_get_atom(env, argv[0], type_str, 16, ERL_NIF_LATIN1)){
    return enif_make_badarg(env);
  }else if(strcmp(type_str,"aes_ecb128")){
    return enif_make_badarg(env);
  }  

  //ARG2 : Key (binary(), must be 16 bits)
  if(!enif_inspect_binary(env, argv[1], &key)) {
    return enif_make_badarg(env);
  }else if(key.size != 16) {
    return enif_make_badarg(env);
  }

  //ARG3 : CipherText (binary()), must be a multiple of 16
  if(!enif_inspect_binary(env, argv[2], &cipher_text)) {
    return enif_make_badarg(env);
  }else if(cipher_text.size % 16) {
    return enif_make_badarg(env);
  }

  unsigned char* decoded = (unsigned char*)malloc(sizeof(unsigned char) * cipher_text.size);

  //Set up decrypt_key
  struct aes_key_st* decrypt_key = (struct aes_key_st*)malloc(sizeof(AES_KEY));
  memset(decrypt_key, 0, sizeof(AES_KEY));
  
  AES_set_decrypt_key((unsigned char*)(key.data), 128, decrypt_key);

  int i = 0;
  for(i = 0; i < cipher_text.size; i += 16) {
    AES_decrypt((unsigned char*)&cipher_text.data[i], (unsigned char*)&decoded[i], decrypt_key);
  }

  //Remove padding
  unsigned char padding = (unsigned char) decoded[cipher_text.size-1];

  if(!enif_alloc_binary(cipher_text.size - padding, &out)) {
    free(decoded);
    free(decrypt_key);
    return enif_make_badarg(env);
  }

  strncpy((unsigned char*)out.data, decoded, cipher_text.size);

  free(decoded);
  free(decrypt_key);
  return enif_make_binary(env, &out);
}

static ErlNifFunc nif_funcs[] =
{
  {"block_encrypt", 3, block_encrypt},
  {"block_decrypt", 3, block_decrypt}
};
ERL_NIF_INIT(crypto_ext,nif_funcs,NULL,NULL,NULL,NULL)