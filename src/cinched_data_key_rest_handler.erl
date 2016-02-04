%%%-------------------------------------------------------------------
%%% @author Mark Steele <mark@control-alt-del.org>
%%% @copyright (C) 2016, Mark Steele
%%% @doc
%%% Cinched data key REST API
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
-module(cinched_data_key_rest_handler).

%% Callbacks for REST API
-export([init/3, known_methods/2,content_types_accepted/2,
         allowed_methods/2, resource_exists/2,
         content_types_provided/2,
         is_authorized/2]).

%% Custom functions
-export([data_key/2]).

-include("cinched.hrl").

-spec init(_,_,_) -> {'upgrade','protocol','cowboy_rest'}.
init(_Transport, _Req, _Args) ->
  {upgrade, protocol, cowboy_rest}.

-spec known_methods(_,_) -> {[binary(),...],_,_}.
known_methods(Req, Ctx) ->
  {[<<"POST">>], Req, Ctx}.

-spec allowed_methods(_,_) -> {[binary(),...],_,_}.
allowed_methods(Req, Ctx) ->
  {[<<"POST">>], Req, Ctx}.

-spec is_authorized(_,_) -> {true,term(),term()}.
is_authorized(Req,Ctx) ->
  {true, Req, Ctx}.

-spec content_types_accepted(_,_) -> {[{'*', data_key}], term(), term()}.
content_types_accepted(Req, Ctx) ->
  {[{'*', data_key}], Req, Ctx}.

-spec content_types_provided(_,_) ->
             {[{{binary(),binary(),'*'}, data_key}], term(), term()}.
content_types_provided(Req, Ctx) ->
  {[{{<<"application">>,<<"json">>,'*'}, data_key}], Req, Ctx}.

-spec resource_exists(_,_) -> {'true',_,_}.
resource_exists(Req, Ctx) ->
  {true, Req, Ctx}.

-spec data_key(_,_) -> {true,term(),{}} | {halt,term(),{}}.
data_key(Req, _Ctx) ->
  Value = cinched:generate_data_key(),
  %% Gather logging data
  {Peer,Port0} = cowboy_req:get(peer,Req),
  Port = integer_to_binary(Port0),
  PeerIP = list_to_binary(inet_parse:ntoa(Peer)),
  {ok,{Serial,PeerCN}} = cinched:get_cert_info_from_socket(cowboy_req:get(socket,Req)),
  {QueryString,_} = cowboy_req:qs(Req),
  {UserAgent,_} = cowboy_req:header(<<"user-agent">>,Req),
  {Metadata,_} = cowboy_req:header(<<"x-cinched-metadata">>,Req),

  case Value of
    {ok, DK} ->
      cinched_log:log([
                       {meta,Metadata},
                       {query_string,QueryString},
                       {user_agent,UserAgent},
                       {peer_ip,PeerIP},
                       {peer_port,Port},
                       {peer_cert_cn,PeerCN},
                       {peer_cert_serial,Serial},
                       {op,data_key},
                       {status,ok}
                      ]),
      Key = base64:encode(term_to_binary(DK,[compressed])),
      CP = integer_to_binary(DK#cinched_key.crypto_period),
      Reply = <<"{\"dataKey\": \"",Key/binary,"\",\"cryptoPeriod\": ",CP/binary,"}">>,
      Req0 = cowboy_req:set_meta('content-type',<<"application/json">>,Req),
      exometer:update([api,data_key,ok],1),
      {true, cowboy_req:set_resp_body(Reply,Req0),{}};
    {error,_} ->
      cinched_log:log([
                       {meta,Metadata},
                       {query_string,QueryString},
                       {user_agent,UserAgent},
                       {peer_ip,PeerIP},
                       {peer_port,Port},
                       {peer_cert_cn,PeerCN},
                       {peer_cert_serial,Serial},
                       {op,data_key},
                       {status,error}
                      ]),
      exometer:update([api,data_key,error],1),
      {halt, Req, {}}
  end.
