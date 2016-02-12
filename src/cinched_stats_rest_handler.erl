%%%-------------------------------------------------------------------
%%% @author Mark Steele <mark@control-alt-del.org>
%%% @copyright (C) 2016, Mark Steele
%%% @doc
%%% Cinched stats REST endpoint
%%% @end
%%% Created : 8 Feb 2016 by Mark Steele <mark@control-alt-del.org>
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
-module(cinched_stats_rest_handler).

%% Callbacks for REST API
-export([init/3, content_types_provided/2]).
-export([stats/2]).

init(_,_,_) ->
  {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, Ctx) ->
  {[{{<<"application">>,<<"json">>,'*'}, stats}], Req, Ctx}.

stats(Req,_Ctx) ->
  case cinched:stats() of
    {ok, Stats} ->
      Req0 = cowboy_req:set_meta('content-type',<<"application/json">>,Req),
      JSON = jiffy:encode({[{format_stat(X),Y} || {X,Y} <- Stats]}),
      {JSON, Req0,{}};
    _ ->
      {halt,Req,{}}
  end.


-spec format_stat([atom()]) -> string().
format_stat(X) ->
  list_to_binary(string:join([format_thing(A) || A <- X ],".")).

-spec format_thing(atom() | integer()) -> list().
format_thing(X) when is_atom(X) ->
  atom_to_list(X);
format_thing(X) when is_integer(X) ->
  integer_to_list(X).
