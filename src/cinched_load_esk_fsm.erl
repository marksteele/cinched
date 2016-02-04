%%%-------------------------------------------------------------------
%%% @author Mark Steele <mark@control-alt-del.org>
%%% @copyright (C) 2016, Mark Steele
%%% @doc
%%% FSM to load the ESK
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
%%
-module(cinched_load_esk_fsm).

-behaviour(gen_fsm).

-include("cinched.hrl").

-export([start_link/0]).

-export([
         init/1,
         load_esk/2,
         load_esk/3,
         wait_shards/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4
        ]).

-define(SERVER, ?MODULE).

-record(state, {esk,shards=[],ensemble,num_shards,threshold}).

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec init([]) -> {ok, load_esk, tuple(),0}.
init([]) ->
  {ok, NumPartitions} = application:get_env(cinched, ring_size),
  Ensemble = cinched:get_ensemble(<<"esk">>, NumPartitions),
  {ok, Shards} = application:get_env(cinched, key_shards),
  {ok, Threshold} = application:get_env(cinched, key_shard_threshold),
  {ok, load_esk, #state{ensemble=Ensemble,num_shards=Shards,threshold=Threshold},0}.

-spec load_esk(timeout,tuple()) -> {next_state,load_esk,tuple(),1000} |
                                   {next_state,wait_shards,tuple()}.
load_esk(timeout,S=#state{ensemble=Ensemble}) ->
  case cinched:status() of
    waiting_ensemble ->
      {next_state, load_esk,S,1000};
    _ ->
      case cinched_keystore:fetch(<<"esk">>,Ensemble) of
        {ok,ESK} ->
          gen_fsm:send_event(cinched_startup_fsm, waiting_shards),
          {next_state, wait_shards, S#state{esk=ESK}};
        _ ->
          gen_fsm:send_event(cinched_startup_fsm, waiting_init),
          {next_state, load_esk, S, 1000}
      end
  end.

-spec load_esk(init,_,tuple()) ->
                  {stop, normal, {ok, list()},tuple()} |
                  {reply, error_retrying, next_state, load_esk, tuple(), 0}.
load_esk(init, _, S=#state{ensemble=Ensemble,threshold=T,num_shards=N}) ->
  SK = nacl:secretbox_key(),
  ShardKey = nacl:secretbox_key(),
  {ok, ESK} = nacl:secretbox(SK, ShardKey),
  Shards = [
            base64:encode(term_to_binary(X))
            || X <- shamir:share(ShardKey, T, N)
           ],
  case cinched_keystore:store(<<"esk">>,Ensemble,ESK) of
    ok ->
      application:set_env(cinched, sk, SK),
      gen_fsm:send_event(cinched_startup_fsm, key_loaded),
      {stop, normal, {ok, Shards}, S};
    _ ->
      %% Error saving to ensemble. Start over.
      {reply, error_retrying, next_state, load_esk, S, 0}
  end.

-spec wait_shards({shard,list()},_,tuple()) ->
                     {stop, normal, key_recovered,tuple()} |
                     {reply, need_more_shards, wait_shards, tuple()} |
                     {next_state, wait_shards, tuple()}.
wait_shards({shard,Shard}, _, S=#state{shards=Shards0,threshold=T,esk=ESK}) ->
  try
    Shards = Shards0 ++ [ binary_to_term(base64:decode(Shard)) ],
    case length(Shards) =:= T of
      true ->
        case decrypt_esk(Shards,ESK) of
          ok ->
            gen_fsm:send_event(cinched_startup_fsm, key_loaded),
            {stop, normal, key_recovered, S};
          _ ->
            {reply, need_more_shards, wait_shards, S#state{shards=[]}}
        end;
      false ->
        {reply, need_more_shards, wait_shards, S#state{shards=Shards}}
    end
  catch
    _:_ ->
      {reply, need_more_shards, wait_shards, S#state{shards=[]}}
  end;
wait_shards(_, _, S) ->
  {next_state, wait_shards, S}.

-spec handle_event(_,atom(),tuple()) -> {next_state,atom(),tuple()}.
handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

-spec handle_sync_event(_,_,atom(),tuple()) -> {reply,ok,atom(),tuple()}.
handle_sync_event(_Event, _From, StateName, State) ->
  {reply, ok, StateName, State}.

-spec handle_info(_,atom(),tuple()) -> {next_state,atom(),tuple()}.
handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

-spec terminate(_,_,_) -> ok.
terminate(_Reason, _StateName, _State) ->
  ok.

-spec code_change(_,atom(),tuple(),_) -> {ok,atom(),tuple()}.
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

-spec decrypt_esk([binary()],tuple()) -> ok | error_recovering_key.
decrypt_esk(Shards,ESK) ->
  try
    ShardKey = shamir:recover(Shards),
    {ok, SK} = nacl:secretbox_open(ESK, ShardKey),
    application:set_env(cinched,sk,SK),
    ok
  catch
    _E:_Err ->
      error_recovering_key
  end.
