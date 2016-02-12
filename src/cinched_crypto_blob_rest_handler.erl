%%%-------------------------------------------------------------------
%%% @author Mark Steele <mark@control-alt-del.org>
%%% @copyright (C) 2016, Mark Steele
%%% @doc
%%% Cinched BLOB REST API
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
-module(cinched_crypto_blob_rest_handler).

-include("cinched.hrl").

%% Callbacks for REST API
-export([init/3, known_methods/2,
         allowed_methods/2,
         content_types_provided/2, is_authorized/2,
         content_types_accepted/2]).

%% Custom functions
-export([encrypt/2, decrypt/2]).

-spec init(_,_,_) -> {'upgrade','protocol','cowboy_rest'}.
init(_Transport, _Req, _Args) ->
  {upgrade, protocol, cowboy_rest}.

-spec known_methods(_,_) -> {[<<_:32>>,...],_,_}.
known_methods(Req, Ctx) ->
  {[<<"POST">>], Req, Ctx}.

-spec allowed_methods(_,_) -> {[<<_:32>>,...],_,_}.
allowed_methods(Req, Ctx) ->
  {[<<"POST">>], Req, Ctx}.

-spec is_authorized(_,_) -> {true,any(),any()}.
is_authorized(Req,Ctx) ->
  {true, Req, Ctx}.

%% TODO: Save this state (the action) for the content types provided function
content_types_accepted(Req, Ctx) ->
  case cowboy_req:binding(action, Req) of
    {undefined,Req} ->
      {halt,Req,{}};
    {<<"encrypt">>,Req} ->
      {[{{<<"application">>, <<"octet-stream">>, '*'}, encrypt}], Req, Ctx};
    {<<"decrypt">>,Req} ->
      {[{{<<"application">>, <<"octet-stream">>, '*'}, decrypt}], Req, Ctx}
  end.

content_types_provided(Req, Ctx) ->
  case cowboy_req:binding(action, Req) of
    {undefined,Req} ->
      {halt,Req,{}};
    {<<"encrypt">>,Req} ->
      {[{{<<"application">>, <<"octet-stream">>, '*'}, encrypt}], Req, Ctx};
    {<<"decrypt">>,Req} ->
      {[{{<<"application">>, <<"octet-stream">>, '*'}, decrypt}], Req, Ctx}
  end.

encrypt(Req0, _Ctx) ->
  {ok, Body, Req1} = cowboy_req:body(Req0,[]),
  {ok, DataKey} = case cowboy_req:header(<<"x-cinched-data-key">>, Req1) of
                    {undefined,Req2} ->
                      cinched:generate_data_key();
                    {KeyHeader,Req2} ->
                      {ok, binary_to_term(base64:decode(KeyHeader))}
                  end,
  {Time, Value} = timer:tc(
                    fun() ->
                        poolboy:transaction(
                          pb,
                          fun(Worker) ->
                              gen_server:call(Worker,{encrypt,DataKey,Body})
                          end
                         )
                    end
                   ),
  %% Gather logging data
  {Peer,Port0} = cowboy_req:get(peer,Req2),
  Port = integer_to_binary(Port0),
  PeerIP = list_to_binary(inet_parse:ntoa(Peer)),
  {ok,{Serial,PeerCN}} = cinched:get_cert_info_from_socket(cowboy_req:get(socket,Req2)),
  {QueryString,Req3} = cowboy_req:qs(Req2),
  {UserAgent,Req4} = cowboy_req:header(<<"user-agent">>,Req3),
  {Metadata,Req5} = cowboy_req:header(<<"x-cinched-metadata">>,Req4),
  exometer:update([crypto_worker,encrypt,time],Time),
  case Value of
    {ok, Response} ->
      cinched_log:log([
                       {op,blob_encrypt},
                       {status,ok},
                       {meta,Metadata},
                       {query_string,QueryString},
                       {user_agent,UserAgent},
                       {peer_ip,PeerIP},
                       {peer_port,Port},
                       {peer_cert_cn,PeerCN},
                       {peer_cert_serial,Serial}
                      ]),
      Req6 = cowboy_req:set_resp_header(
               <<"x-cinched-data-key">>,
               base64:encode(term_to_binary(DataKey)),
               Req5
              ),
      Req7 = cowboy_req:set_resp_header(
               <<"x-cinched-crypto-period">>,
               integer_to_binary(DataKey#cinched_key.crypto_period),
               Req6
              ),
      exometer:update([api,blob,encrypt,ok],1),
      {true, cowboy_req:set_resp_body(Response, Req7), {}};
    {error,_Err} ->
      cinched_log:log([
                       {op,blob_encrypt},
                       {status,error},
                       {meta,Metadata},
                       {query_string,QueryString},
                       {user_agent,UserAgent},
                       {peer_ip,PeerIP},
                       {peer_port,Port},
                       {peer_cert_cn,PeerCN},
                       {peer_cert_serial,Serial}
                      ]),
      exometer:update([api,blob,encrypt,error],1),
      {false, Req5, {}}
  end.

-spec decrypt(_,_) -> {'true',_,{}}.
decrypt(Req0, _Ctx) ->
  {ok, Body, Req1} = cowboy_req:body(Req0,[]),
  {ok, DataKey} = case cowboy_req:header(<<"x-cinched-data-key">>, Req1) of
                    {undefined,Req2} ->
                      throw("Error, missing data key");
                    {KeyHeader,Req2} ->
                      {ok, binary_to_term(base64:decode(KeyHeader))}
                  end,
  {Time, Value} = timer:tc(
                    fun() ->
                        poolboy:transaction(
                          pb,
                          fun(Worker) ->
                              gen_server:call(Worker,{decrypt,DataKey,Body})
                          end
                         )
                    end
                   ),
  exometer:update([crypto_worker,decrypt,time],Time),

  %% Gather logging data
  {Peer,Port0} = cowboy_req:get(peer,Req2),
  Port = integer_to_binary(Port0),
  PeerIP = list_to_binary(inet_parse:ntoa(Peer)),
  {ok,{Serial,PeerCN}} = cinched:get_cert_info_from_socket(cowboy_req:get(socket,Req2)),
  {QueryString,Req3} = cowboy_req:qs(Req2),
  {UserAgent,Req4} = cowboy_req:header(<<"user-agent">>,Req3),
  {Metadata,Req5} = cowboy_req:header(<<"x-cinched-metadata">>,Req4),

  case Value of
    {ok, Response} ->
      cinched_log:log([
                       {op,blob_decrypt},
                       {status,ok},
                       {meta,Metadata},
                       {query_string,QueryString},
                       {user_agent,UserAgent},
                       {peer_ip,PeerIP},
                       {peer_port,Port},
                       {peer_cert_cn,PeerCN},
                       {peer_cert_serial,Serial}
                      ]),
      exometer:update([api,blob,decrypt,ok],1),
      {true,cowboy_req:set_resp_body(Response,Req5), {}};
    {error,_} ->
      cinched_log:log([
                       {op,blob_decrypt},
                       {status,error},
                       {meta,Metadata},
                       {query_string,QueryString},
                       {user_agent,UserAgent},
                       {peer_ip,PeerIP},
                       {peer_port,Port},
                       {peer_cert_cn,PeerCN},
                       {peer_cert_serial,Serial}

                      ]),
      exometer:update([api,blob,decrypt,error],1),
      {halt, Req5, {}}
  end.
