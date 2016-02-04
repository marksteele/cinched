%%%-------------------------------------------------------------------
%%% @author Mark Steele <mark@control-alt-del.org>
%%% @copyright (C) 2016, Mark Steele
%%% @doc
%%% Cinched riak_ensemble backend
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
-module(cinched_ensemble_backend).
-behaviour(riak_ensemble_backend).

-export([init/3, new_obj/4]).
-export([obj_epoch/1, obj_seq/1, obj_key/1, obj_value/1]).
-export([set_obj_epoch/2, set_obj_seq/2, set_obj_value/2]).
-export([get/3, put/4, tick/5, ping/2, ready_to_start/0]).
-export([synctree_path/2]).
-export([handle_down/4]).

-include("cinched.hrl").

-record(state,{leveldb_server}).

-type obj()   :: #obj{}.
-type state() :: #state{}.
-type key()   :: any().
-type value() :: any().

-spec init(non_neg_integer(),_,[]) -> tuple().
init(Ensemble, _Id, []) ->
  ChildId = list_to_atom("leveldb_"++integer_to_list(Ensemble)++"_sup"),
  {ok, _Pid} = supervisor:start_child(
                    cinched_sup,
                    {
                      ChildId,
                      {
                        cinched_eleveldb,
                        start_link,
                        [Ensemble]
                      },
                      permanent,
                      10000,
                      worker,
                      [cinched_eleveldb]
                    }
                   ),
  LevelDb = list_to_atom("leveldb_"++integer_to_list(Ensemble)),
  #state{leveldb_server=LevelDb}.

-spec new_obj(term(),term(),term(),term()) -> obj().
new_obj(Epoch, Seq, Key, Value) ->
  #obj{epoch=Epoch, seq=Seq, key=Key, value=Value}.

-spec obj_epoch(obj()) -> integer().
obj_epoch(Obj) ->
  Obj#obj.epoch.

-spec obj_seq(obj()) -> integer().
obj_seq(Obj) ->
  Obj#obj.seq.

-spec obj_key(obj()) -> key().
obj_key(Obj) ->
  Obj#obj.key.

-spec obj_value(obj()) -> value().
obj_value(Obj) ->
  Obj#obj.value.

-spec set_obj_epoch(term(),obj()) -> obj().
set_obj_epoch(Epoch, Obj) ->
  Obj#obj{epoch=Epoch}.

-spec set_obj_seq(term(),obj()) -> obj().
set_obj_seq(Seq, Obj) ->
  Obj#obj{seq=Seq}.

-spec set_obj_value(value(), obj()) -> obj().
set_obj_value(Value, Obj) ->
  Obj#obj{value=Value}.

-spec get(binary(),{{_,_} | {'riak_ensemble_msg',pid(),reference()},{_,atom()}},tuple()) -> tuple().
get(Key, From, State=#state{leveldb_server=Server}) ->
  gen_server:cast(Server,{get,Key,From}),
  State.

-spec put(binary(),obj(),{{_,_} | {'riak_ensemble_msg',pid(),reference()},{_,atom()}},tuple()) -> tuple().
put(Key, Obj, From, State=#state{leveldb_server=Server}) ->
  gen_server:cast(Server,{put,Key,Obj,From}),
  State.

-spec tick(_,_,_,_,tuple()) -> tuple().
tick(_Epoch, _Seq, _Leader, _Views, State) ->
  State.

-spec ping(pid(), state()) -> {ok, state()}.
ping(_From, State) ->
  {ok, State}.

-spec ready_to_start() -> true.
ready_to_start() ->
  true.

-spec synctree_path(_,_) -> default.
synctree_path(_Ensemble, _Id) ->
  default.

-spec handle_down(reference(), pid(), term(), state()) -> false.
handle_down(_Ref, _Pid, _Reason, _State) ->
  false.
