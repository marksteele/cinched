%%%-------------------------------------------------------------------
%%% @author Mark Steele <mark@control-alt-del.org>
%%% @copyright (C) 2016, Mark Steele
%%% @doc
%%% Cinched crypto poolboy worker
%%% @end
%%% Created : 24 Jan 2016 by Mark Steele <mark@control-alt-del.org>
%%%-------------------------------------------------------------------
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
-module(cinched_crypto_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-include("cinched.hrl").

-export([
         start_link/1,
         encrypt/3,
         decrypt/3,
         encrypt/2
        ]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(KEY_VERSION, 1).

-record(state, {sk,ring_size}).

-spec start_link(_) -> {ok, pid()} | {error, term()}.
start_link(_Args) ->
    gen_server:start_link(?MODULE, [], []).

-spec init([]) -> {ok, tuple()}.
init([]) ->
  {ok, SK} = application:get_env(cinched, sk),
  {ok, RingSize} = application:get_env(cinched, ring_size),
  {ok, #state{sk=SK,ring_size=RingSize}}.

-spec handle_call(term(),term(),term()) ->
                     {'noreply',_} |
                     {'noreply',_,'hibernate' | 'infinity' | non_neg_integer()} |
                     {'reply',_,_} | {'stop',_,_} |
                     {'reply',_,_,'hibernate' | 'infinity' | non_neg_integer()} |
                     {'stop',_,_,_}.
handle_call(data_key, _From, S=#state{sk=SK,ring_size=RingSize}) ->
  try
    CryptoPeriod = generate_crypto_period(),
    {ok, MasterKey} = load_key(CryptoPeriod,SK,RingSize),
    {ok, EncryptedDataKey} = cinched_crypto:encrypt(MasterKey,cinched_crypto:key()),
    exometer:update([crypto_worker,data_key,ok],1),
    {reply, {ok,
             #cinched_key{
                key = EncryptedDataKey,
                version = ?KEY_VERSION,
                crypto_period = CryptoPeriod
               }
            },
     S
    }
  catch
    _E:Err ->
      exometer:update([crypto_worker,generate_eek,error],1),
      {reply, {error, Err}, S}
  end;

handle_call({Op, EDK, Payload, Fields}, _From, S=#state{sk=SK,ring_size=RingSize}) ->
  try
    {ok, DataKey} = load_data_key(EDK,RingSize,SK),
    {ok, Data} = ?MODULE:Op(Payload, DataKey, Fields),
    exometer:update([crypto_worker,Op,ok],1),
    {reply, {ok, Data}, S}
  catch
    _E:Err ->
      exometer:update([crypto_worker,Op,error],1),
      {reply,{error, Err}, S}
  end;

handle_call({Op, EDK, Payload}, _From, S=#state{sk=SK,ring_size=RingSize}) ->
  try
    {ok, DataKey} = load_data_key(EDK,RingSize,SK),
    {ok, Data} = ?MODULE:Op(Payload, DataKey),
    exometer:update([crypto_worker,Op,ok],1),
    {reply, {ok, Data}, S}
  catch
    _E:Err ->
      exometer:update([crypto_worker,Op,error],1),
      {reply,{error, Err}, S}
  end.

-spec handle_cast(_,_) ->
                     {'noreply',_} |
                     {'noreply',_,'hibernate' | 'infinity' | non_neg_integer()} |
                     {'stop',_,_}.
handle_cast(_Msg, S) ->
  {noreply, S}.

-spec handle_info(_,_) -> {'noreply',_} |
                          {'noreply',_,'hibernate' | 'infinity' | non_neg_integer()} |
                          {'stop',_,_}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(_,_) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(_,_,_) -> {ok,term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec load_key(non_neg_integer(),binary(),1..1024) -> {ok,term()} | {error,key_error}.
load_key(CryptoPeriod,SK,RingSize) ->
  case load_key_cache(CryptoPeriod) of
    {ok, Key} ->
      exometer:update([crypto_worker,load_key,ok],1),
      {ok, Key};
    _ ->
      Ensemble = cinched:get_ensemble(CryptoPeriod, RingSize),
      case load_or_generate_master_key({CryptoPeriod,Ensemble},SK) of
        {ok, MasterKey} ->
          exometer:update([crypto_worker,load_key,ok],1),
          cache:put(key_cache,CryptoPeriod,MasterKey),
          {ok, MasterKey};
        _Error ->
          exometer:update([crypto_worker,load_key,error],1),
          {error, key_error}
      end
  end.

-spec generate_master_key({non_neg_integer(),non_neg_integer()},binary()) -> {ok,term()} | error.
generate_master_key({CryptoPeriod,Ensemble}, SK) ->
  MasterKey = cinched_crypto:key(),
  {ok, EncryptedMasterKey} = cinched_crypto:encrypt(SK,MasterKey),
  KeyObj = #cinched_key{
         key=EncryptedMasterKey,
         crypto_period=CryptoPeriod,
         version=?KEY_VERSION
        },
  case cinched_keystore:store(CryptoPeriod,Ensemble,KeyObj) of
    ok ->
      {ok, MasterKey};
    error ->
      error
  end.

-spec load_or_generate_master_key({non_neg_integer(),1..1024},binary()) -> {ok,term()} | error.
load_or_generate_master_key({CryptoPeriod,Ensemble}, SK) when is_integer(CryptoPeriod) ->
  case cinched_keystore:fetch(CryptoPeriod,Ensemble) of
    {error, not_found} ->
      generate_master_key({CryptoPeriod,Ensemble}, SK);
    {ok, EncryptedMasterKey} ->
      cinched_crypto:decrypt(SK,EncryptedMasterKey#cinched_key.key);
    {error, _} ->
      error
  end.

-spec load_key_cache(non_neg_integer()) -> {error,cache_miss} | {ok, term()}.
load_key_cache(CryptoPeriod) ->
  case cache:get(key_cache, CryptoPeriod) of
    undefined ->
      {error, cache_miss};
    Key ->
      {ok, Key}
  end.

-spec load_data_key(term(),1..1024,binary()) -> {ok,term()}.
load_data_key(EDK=#cinched_key{crypto_period=CryptoPeriod}, RingSize, SK) ->
  {ok, MK} = case load_key_cache(CryptoPeriod) of
               {error, cache_miss} ->
                 Ensemble = cinched:get_ensemble(CryptoPeriod,RingSize),
                 case cinched_keystore:fetch(CryptoPeriod,Ensemble) of
                   {ok, EMK} ->
                     decrypt_key(EMK#cinched_key.key,SK);
                   _ ->
                     error
                 end;
               MasterKey ->
                 MasterKey
             end,
  decrypt_key(EDK#cinched_key.key, MK).

-spec encrypt(binary(),binary(),list()) -> {ok,binary()}.
encrypt(Payload, Key, Fields) ->
  O = jiffy:decode(Payload),
  Ret = lists:foldl(
          fun(X,Acc) ->
              case ej:get(X,Acc) of
                undefined ->
                  throw("Undefined field specified");
                Value ->
                  {ok, Encrypted} = cinched_crypto:encrypt(Key,term_to_binary(Value)),
                  ej:set(X,Acc,base64:encode(term_to_binary(Encrypted)))
              end
          end,
          O,
          Fields),
  {ok, jiffy:encode(Ret)}.

-spec encrypt(binary(),binary()) -> {ok, binary()}.
encrypt(Payload, Key) ->
  case cinched_crypto:encrypt(Key,Payload) of
    {ok, Encrypted} ->
      {ok, term_to_binary(Encrypted)};
    {error, Error} ->
      {error, Error}
  end.

-spec decrypt(binary(),binary(),list()) -> {ok,binary()}.
decrypt(Payload, Key, Fields) ->
  O = jiffy:decode(Payload),
  Ret = lists:foldl(
          fun(X,Acc) ->
              case ej:get(X,Acc) of
                undefined ->
                  throw("Undefined field specified");
                Value ->
                  {ok, Decrypted} = cinched_crypto:decrypt(Key,binary_to_term(base64:decode(Value))),
                  ej:set(X,Acc,binary_to_term(Decrypted))
              end
          end,
          O,
          Fields),
  {ok, jiffy:encode(Ret)}.

-spec decrypt_key(term(), binary()) -> {error,term()} | {ok,term()}.
decrypt_key(Payload,Key) ->
  case cinched_crypto:decrypt(Key,Payload) of
    {error, Error} ->
      exometer:update([crypto_worker,decrypt,error],1),
      {error, Error};
    Data ->
      exometer:update([crypto_worker,decrypt,ok],1),
      Data
  end.

-spec generate_crypto_period() -> non_neg_integer().
generate_crypto_period() ->
  trunc(cinched:unix_timestamp()/86400).
