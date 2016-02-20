-module(cinched_crypto).

-include("cinched.hrl").

-export([encrypt/2, decrypt/2, hash/1, key/0]).

hash(Input) when is_list(Input) ->
  hash(iolist_to_binary(Input));
hash(Input) when is_binary(Input) ->
  Size = enacl:secretbox_key_size(),
  hash(Input, Size, <<>>).

hash(_, Size, Acc) when size(Acc) >= Size->
  <<Hash:Size/binary, _/binary>> = Acc,
  Hash;
hash(Input, Size, Acc) ->
  More = enacl:hash(<<Acc/binary, Input/binary>>),
  hash(Input, Size,  <<Acc/binary, More/binary>>).

key() ->
  enacl:randombytes(enacl:secretbox_key_size()).

encrypt(Key,Payload) when is_binary(Key) and is_binary(Payload) ->
  Nonce = enacl:randombytes(enacl:secretbox_nonce_size()),
  Ciphertext = enacl:secretbox(Payload,Nonce,Key),
  {ok, #nacl_envelope{nonce=Nonce,ciphertext=Ciphertext}}.

decrypt(Key,#nacl_envelope{nonce=Nonce,ciphertext=Ciphertext}) when
    is_binary(Key) and is_binary(Ciphertext) ->
  enacl:secretbox_open(Ciphertext,Nonce,Key).
