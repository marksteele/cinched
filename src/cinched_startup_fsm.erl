%%%-------------------------------------------------------------------
%%% @author Mark Steele <mark@control-alt-del.org>
%%% @copyright (C) 2016, Mark Steele
%%% @doc
%%% Cinched start-up FSM. Coordinates and tracks the startup sequence.
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
-module(cinched_startup_fsm).

-behaviour(gen_fsm).

-include("cinched.hrl").

%% API
-export([start_link/0]).

-export([
         init/1,
         waiting_ensemble/2,
         waiting_shards/2,
         waiting_init/2,
         waiting_sk/2,
         started/2,
         starting_services/2,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4
        ]).

-define(SERVER, ?MODULE).
-include_lib("public_key/include/public_key.hrl").

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec init([]) -> {ok, waiting_ensemble,{}}.
init([]) ->
  {ok, waiting_ensemble, {}}.

%% Normal startup sequence
waiting_ensemble(ensemble_up,{}) ->
  {next_state, waiting_sk, {}};
%% If startup FSM crashes events might come in out of order
waiting_ensemble(key_loaded,{}) ->
  {next_state, starting_services,{},0}.

%% Ensemble or startup FSM crash
waiting_sk(ensemble_up,{}) ->
  {next_state, waiting_sk, {}};
%% ESK or startup fsm crash
waiting_sk(key_loaded,{}) ->
  {next_state,starting_services,{},0};
%% SK not found
waiting_sk(waiting_init,{}) ->
  {next_state,waiting_init,{}};
%% SK found
waiting_sk(waiting_shards,{}) ->
  {next_state,waiting_shards,{}}.

%% SK not found
waiting_init(waiting_init,S) ->
  {next_state,waiting_init,S};
%% SK found (initialized from another node)
waiting_init(waiting_shards,S) ->
  {next_state,waiting_shards,S};
%% Initialized on this node
waiting_init(key_loaded,S) ->
  {next_state,starting_services,S,0}.

%% Ensemble/Startup FSM crash
waiting_shards(ensemble_up,{}) ->
  {next_state, waiting_shards,{}};
waiting_shards(key_loaded,S) ->
  {next_state,starting_services,S,0}.

-spec starting_services(timeout,{}) -> {next_state, started,{}}.
starting_services(timeout,S) ->
  ok = register_metrics(),
  ok = register_cache(),
  ok = start_workers(),
  ok = start_audit_log(),
  ok = start_cowboy(),
  ok = start_stats_http(),
  {next_state, started, S}.

started(_,{}) ->
  {next_state,started,{}}.

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(status,_,StateName,S) ->
  {reply, StateName, StateName, S}.

-spec handle_info(_,atom(),{}) -> {next_state,atom(),{}}.
handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

-spec terminate(_,atom(),{}) -> ok.
terminate(_Reason, _StateName, _State) ->
  ok.

-spec code_change(_,atom(),{},_) -> {ok,atom(),{}}.
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

-spec start_workers() -> ok.
start_workers() ->
  case is_pid(whereis(pb)) of
    false ->
      {ok, PoolSize} = application:get_env(cinched, workers),
      {ok, PoolOverflow} = application:get_env(cinched, workers_overflow),
      Args = [{name, {local, pb}},{size,PoolSize},{max_overflow,PoolOverflow},
              {worker_module, cinched_crypto_worker}],
      Spec = {pb, {poolboy, start_link, [Args]},
              permanent, 5000, worker, [poolboy]},
      {ok, _Pid} = supervisor:start_child(cinched_sup, Spec);
    true ->
      ok
  end,
  ok.

-spec start_audit_log() -> ok.
start_audit_log() ->
  case is_pid(whereis(cinched_audit_sup)) of
    false ->
      {ok, _ChildKS} = supervisor:start_child(
                         cinched_sup,
                         {
                           cinched_audit_sup,
                           {
                             cinched_log,
                             start_link,
                             []
                           },
                           permanent,
                           10000,
                           worker,
                           [cinched_log]
                         }
                        );
    true ->
      ok
  end,
  ok.

start_stats_http() ->
  case is_pid(whereis(cinched_cb_stats_sup)) of
    false ->
      {ok, Interface} = application:get_env(cinched, ip),
      {ok, Port} = application:get_env(cinched, stats_http_port),
      {ok, ParsedInterface} = inet:parse_address(Interface),

      Dispatch = cowboy_router:compile(
                   [
                    {'_',
                     [
                      {<<"/stats">>, cinched_stats_rest_handler, []}
                     ]
                    }
                   ]
                  ),
      CBOptions = [
                   {env, [{dispatch, Dispatch}]},
                   {max_keepalive,100}
                  ],
      RanchOptions = [
                      {ip, ParsedInterface},
                      {port, Port}
                     ],
      {ok, _} = supervisor:start_child(
                  cinched_sup,
                  {
                    cinched_cb_stats_sup,
                    {
                      cowboy,
                      start_http,
                      [http, 50, RanchOptions, CBOptions]
                    },
                    permanent,
                    brutal_kill,
                    supervisor,
                    [cowboy]
                  }
                 );
    true ->
      ok
  end,
  ok.

%% @doc Starts the webserver
-spec start_cowboy() -> ok.
start_cowboy() ->
  case is_pid(whereis(cinched_cb_sup)) of
    false ->
      {ok, Interface} = application:get_env(cinched, ip),
      {ok, Port} = application:get_env(cinched, tls_port),
      {ok, HTTPAcceptors} = application:get_env(cinched, http_acceptors),
      {ok, ParsedInterface} = inet:parse_address(Interface),

      {ok, CACertBin} = file:read_file(?CACERTFILE),

      Dispatch = cowboy_router:compile(
                   [
                    {'_',
                     [
                      {<<"/doc/[:action]">>, cinched_crypto_field_rest_handler, []},
                      {<<"/blob/[:action]">>, cinched_crypto_blob_rest_handler, []},
                      {<<"/key/data-key">>, cinched_data_key_rest_handler, []}
                     ]
                    }
                   ]
                  ),
      CBOptions = [{env, [{dispatch, Dispatch}]},{max_keepalive,100}],
      RanchOptions = [
                      {nodelay,true},
                      {cacertfile,?CACERTFILE},
                      {certfile,?CERTFILE},
                      {keyfile,?KEYFILE},
                      {server_name_indication,false},
                      {depth,0},
                      {crl_check,peer},
                      {ip, ParsedInterface},
                      {port, Port},
                      {verify, verify_peer},
                      {verify_fun, {fun validate_function/3,binary_to_list(CACertBin)}},
                      {fail_if_no_peer_cert,true},
                      {reuse_sessions, true},
                      {secure_renegotiate,true},
                      {honor_cipher_order, true},
                      {versions,['tlsv1.2']},
                      {ciphers,
                       [
                        "ECDHE-ECDSA-AES256-SHA384",
                        "ECDHE-RSA-AES256-SHA384",
                        "ECDH-ECDSA-AES256-SHA384",
                        "ECDH-RSA-AES256-SHA384",
                        "DHE-RSA-AES256-SHA256",
                        "DHE-DSS-AES256-SHA256",
                        %% "AES256-SHA256",
                        "ECDHE-ECDSA-AES128-SHA256",
                        "ECDHE-RSA-AES128-SHA256",
                        "ECDH-ECDSA-AES128-SHA256",
                        "ECDH-RSA-AES128-SHA256",
                        "DHE-RSA-AES128-SHA256",
                        "DHE-DSS-AES128-SHA256"
                        %% "AES128-SHA256",
                        %% "ECDHE-ECDSA-AES256-SHA",
                        %% "ECDHE-RSA-AES256-SHA",
                        %% "DHE-RSA-AES256-SHA",
                        %% "DHE-DSS-AES256-SHA",
                        %% "ECDH-ECDSA-AES256-SHA",
                        %% "ECDH-RSA-AES256-SHA",
                        %% "AES256-SHA",
                        %% "ECDHE-ECDSA-DES-CBC3-SHA",
                        %% "ECDHE-RSA-DES-CBC3-SHA",
                        %% "EDH-RSA-DES-CBC3-SHA",
                        %% "EDH-DSS-DES-CBC3-SHA",
                        %% "ECDH-ECDSA-DES-CBC3-SHA",
                        %% "ECDH-RSA-DES-CBC3-SHA",
                        %% "ECDHE-ECDSA-AES128-SHA",
                        %% "ECDHE-RSA-AES128-SHA",
                        %% "DHE-RSA-AES128-SHA",
                        %% "DHE-DSS-AES128-SHA",
                        %% "ECDH-ECDSA-AES128-SHA",
                        %% "ECDH-RSA-AES128-SHA",
                        %% "AES128-SHA"
                       ]
                      }
                     ],
      {ok, _} = supervisor:start_child(
                  cinched_sup,
                  {
                    cinched_cb_sup,
                    {
                      cowboy,
                      start_https,
                      [https, HTTPAcceptors, RanchOptions, CBOptions]
                    },
                    permanent,
                    brutal_kill,
                    supervisor,
                    [cowboy]
                  }
                 );
    true ->
      ok
  end,
  ok.

%% @doc Register exometer metrics. Should move this?
-spec register_metrics() -> 'ok'.
register_metrics() ->

  exometer:re_register([api,blob,encrypt,error],counter,[]),
  exometer:re_register([api,blob,decrypt,error],counter,[]),
  exometer:re_register([api,blob,encrypt,ok],counter,[]),
  exometer:re_register([api,blob,decrypt,ok],counter,[]),

  exometer:re_register([api,field,encrypt,ok],counter,[]),
  exometer:re_register([api,field,decrypt,ok],counter,[]),
  exometer:re_register([api,field,encrypt,error],counter,[]),
  exometer:re_register([api,field,decrypt,error],counter,[]),

  exometer:re_register([api,data_key,ok],counter,[]),
  exometer:re_register([api,data_key,error],counter,[]),

  exometer:re_register([crypto_worker,encrypt,error],counter,[]),
  exometer:re_register([crypto_worker,encrypt,ok],counter,[]),
  exometer:re_register([crypto_worker,decrypt,error],counter,[]),
  exometer:re_register([crypto_worker,decrypt,ok],counter,[]),
  exometer:re_register([crypto_worker,load_key,ok],counter,[]),
  exometer:re_register([crypto_worker,load_key,error],counter,[]),

  exometer:re_register([crypto_worker,encrypt,time],histogram,[]),
  exometer:re_register([crypto_worker,decrypt,time],histogram,[]),

  exometer:re_register([crypto_worker,data_key,time], histogram,[]),

  exometer:re_register([keystore,get,ok],counter,[]),
  exometer:re_register([keystore,get,not_found],counter,[]),
  exometer:re_register([keystore,get,error],counter,[]),
  exometer:re_register([keystore,put,ok],counter,[]),
  exometer:re_register([keystore,put,error],counter,[]),

  exometer:re_register([cache,ocsp_cache,put],counter,[]),
  exometer:re_register([cache,ocsp_cache,hit],counter,[]),
  exometer:re_register([cache,ocsp_cache,miss],counter,[]),
  exometer:re_register([cache,key_cache,put],counter,[]),
  exometer:re_register([cache,key_cache,hit],counter,[]),
  exometer:re_register([cache,key_cache,miss],counter,[]),

  exometer:re_register([ocsp,lookup,time], histogram,[]),
  exometer:re_register([ocsp,check,good],counter,[]),
  exometer:re_register([ocsp,check,revoked],counter,[]),
  exometer:re_register([ocsp,check,error],counter,[]),
  exometer:re_register([ocsp,check,timeout],counter,[]),

  exometer:re_register(
    [erlang,system_info],
    {
      function,
      erlang,
      system_info,
      ['$dp'],
      value,
      [
       port_count,
       process_count,
       thread_pool_size
      ]
    },[]
   ),
  exometer:re_register(
    [vm,erlang],
    {
      function,
      erlang,
      memory,
      ['$dp'],
      value,
      [
       total,
       processes,
       processes_used,
       system,
       atom,
       atom_used,
       binary,
       ets
      ]
    },[]
   ),
  exometer:re_register(
    [cowboy],
    {
      function,
      cinched,
      cowboy_stats,
      ['$dp'],
      value,
      [active_connections]
    },[]
   ),
  ok.

%% @doc Register the memory caches (master keys, OCSP responses)
-spec register_cache() -> 'ok'.
register_cache() ->
  StatsFun = fun({Name,Metric,Operation}) ->
                 exometer:update([Name,Metric,Operation],1)
             end,
  case is_pid(whereis(key_cache_sup)) of
    false ->
      {ok, CacheSize} = application:get_env(cinched, key_cache_size),
      {ok, CacheTTL} = application:get_env(cinched, key_cache_ttl),
      {ok, CacheSegments} = application:get_env(cinched, key_cache_segments),
      {ok, _} = supervisor:start_child(
                  cinched_sup,
                  {
                    key_cache_sup,
                    {
                      cache,
                      start_link,
                      [key_cache,[
                                  {n, CacheSegments},
                                  {ttl, CacheTTL},
                                  {memory, CacheSize},
                                  {stats, StatsFun},
                                  {policy,lru}
                                 ]]
                    },
                    permanent,
                    brutal_kill,
                    worker,
                    [cache]
                  }
                 );
    true ->
      ok
  end,
  case is_pid(whereis(ocsp_cache_sup)) of
    false ->
      {ok, OCSPCacheSize} = application:get_env(cinched, ocsp_cache_size),
      {ok, OCSPCacheTTL} = application:get_env(cinched, ocsp_cache_ttl),
      {ok, OCSPCacheSegments} = application:get_env(cinched, ocsp_cache_segments),
      {ok, _} = supervisor:start_child(
                  cinched_sup,
                  {
                    ocsp_cache_sup,
                    {
                      cache,
                      start_link,
                      [ocsp_cache,[
                                   {n, OCSPCacheSegments},
                                   {ttl, OCSPCacheTTL},
                                   {memory, OCSPCacheSize},
                                   {stats, StatsFun},
                                   {policy,lru}
                                  ]
                      ]
                    },
                    permanent,
                    brutal_kill,
                    worker,
                    [cache]
                  }
                 );
    true ->
      ok
  end,
  ok.

%% @doc This is a TLS validation function used by cowboy to validate
%%      the peer x509 cert during TLS negotiation.
-spec validate_function(binary() | tuple(),
                        valid_peer, binary()) ->
                           {fail, term()} | {valid, term()} | {unknown, term()}.
validate_function(Cert, valid_peer, CACert) ->
  {ok, {Serial,_}} = public_key:pkix_issuer_id(Cert, self),
  case cache:get(ocsp_cache,Serial) of
    undefined ->
      %% Hit OCSP
      Der = public_key:pkix_encode('OTPCertificate',Cert,otp),
      PeerPem = der_to_pem(Der),
      {Time, Value} = timer:tc(
                    fun() ->
                        ocsperl:check(PeerPem, CACert, CACert)
                    end
                   ),
      exometer:update([ocsp,lookup,time], Time),
      case Value of
        error ->
          exometer:update([ocsp,check,error],1),
          {fail, CACert};
        revoked ->
          exometer:update([ocsp,check,revoked],1),
          cache:put(ocsp_cache,Serial,revoked),
          {fail, CACert};
        good ->
          exometer:update([ocsp,check,good],1),
          cache:put(ocsp_cache,Serial,good),
          {valid, CACert}
      end;
    revoked ->
      exometer:update([ocsp,check,revoked],1),
      {fail, CACert};
    good ->
      exometer:update([ocsp,check,good],1),
      {valid, CACert}
  end;
validate_function(_Cert, valid, State) ->
  {valid, State};
validate_function(_, {bad_cert, _}=Reason, _) ->
  {fail, Reason};
validate_function(_, {extension,_},State) ->
  {unknown, State}.

%% @doc Given a der encoded certificate, spit out a PEM encoded
%%      certificate.
-spec der_to_pem(binary()) -> list().
der_to_pem(Bin) ->
  Encoded = split_lines(base64:encode(Bin)),
  binary_to_list(
    <<
      "-----BEGIN CERTIFICATE-----\n",
      Encoded/binary,
      "-----END CERTIFICATE-----\n\n"
    >>
   ).

%% @doc Split input to a given length, adding a separator.
-spec split_lines(binary()) -> binary().
split_lines(Text) ->
  split_lines(Text,<<>>).

%% Buffer larger than line length
-spec split_lines(binary(),binary()) -> binary().
split_lines(<<Text:?PEM_ENCODED_LINE_LENGTH/binary, Rest/binary>>, Acc) ->
  split_lines(Rest, <<Acc/binary,Text/binary, "\n">>);
%% Buffer exactly line length (last line)
split_lines(<<Text:?PEM_ENCODED_LINE_LENGTH/binary>>, Acc) ->
  <<Acc/binary,Text/binary, "\n">>;
%% Buffer smaller than line length (last line)
split_lines(Text,Acc) ->
  <<Acc/binary,Text/binary, "\n">>.
