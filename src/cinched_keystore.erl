%%%-------------------------------------------------------------------
%%% @author Mark Steele <mark@control-alt-del.org>
%%% @copyright (C) 2016, Mark Steele
%%% @doc
%%% Cinched key storage abstraction
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
-module(cinched_keystore).

-include("cinched.hrl").

-export([
         fetch/2,
         store/3,
         overwrite/3
        ]).
%% API

-spec fetch(term(),non_neg_integer()) ->
               {error, not_found | ensemble_error} |
               {ok, term()}.
fetch(Key, Ensemble) ->
  case riak_ensemble_client:kget(node(),Ensemble,cinched:hash(Key),10000) of
    {ok,#obj{value=notfound}} ->
      exometer:update([keystore,get,not_found],1),
      {error, not_found};
    {ok,Obj} ->
      exometer:update([keystore,get,ok],1),
      {ok, binary_to_term(Obj#obj.value)};
    _ ->
      exometer:update([keystore,get,error],1),
      {error, ensemble_error}
  end.

-spec store(term(),non_neg_integer(),term()) ->
               ok | error.
store(Key, Ensemble, Value) ->
  case riak_ensemble_client:kput_once(
         node(),
         Ensemble,
         cinched:hash(Key),
         term_to_binary(Value),
         10000
        ) of
    {ok, _} ->
      exometer:update([keystore,put,ok],1),
      ok;
    _ ->
      exometer:update([keystore,put,error],1),
      error
  end.

-spec overwrite(term(),non_neg_integer(),term()) ->
               ok | error.
overwrite(Key, Ensemble, Value) ->
  case riak_ensemble_client:kover(
         node(),
         Ensemble,
         cinched:hash(Key),
         term_to_binary(Value),
         10000
        ) of
    {ok, _} ->
      exometer:update([keystore,overwrite,ok],1),
      ok;
    _ ->
      exometer:update([keystore,overwrite,error],1),
      error
  end.
