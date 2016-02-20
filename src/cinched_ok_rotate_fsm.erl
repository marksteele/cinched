%%%-------------------------------------------------------------------
%%% @author Mark Steele <mark@control-alt-del.org>
%%% @copyright (C) 2016, Mark Steele
%%% @doc
%%% Cinched FSM to rotate the operator key (OK) and generate
%%% a new set of key shards.
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
-module(cinched_ok_rotate_fsm).

-behaviour(gen_fsm).

-export([start_link/0]).

-export([init/1,
         wait_shards/2,
         wait_shards/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-define(SERVER, ?MODULE).
-define(TIMEOUT,600000).

-record(state, {shards=[],threshold,ensemble,num_shards,sk,esk}).

-spec start_link() -> {ok,pid()}.
start_link() ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec init([]) -> {ok, wait_shards,tuple(),?TIMEOUT}.
init([]) ->
  {ok, NumPartitions} = application:get_env(cinched, ring_size),
  Ensemble = cinched:get_ensemble(<<"esk">>, NumPartitions),
  {ok, Shards} = application:get_env(cinched, key_shards),
  {ok, Threshold} = application:get_env(cinched, key_shard_threshold),
  {ok, SK} = application:get_env(cinched,sk),
  {ok, ESK} = cinched_keystore:fetch(<<"esk">>,Ensemble),
  {ok,
   wait_shards,
   #state{
      ensemble=Ensemble,
      num_shards=Shards,
      threshold=Threshold,
      sk=SK,
      esk=ESK
     },
   ?TIMEOUT
  }.

-spec wait_shards(_,tuple()) -> {stop,timeout,tuple()}.
wait_shards(_,S) ->
  {stop,timeout,S}.

-spec wait_shards({shard,binary()},_,tuple()) ->
                     {stop, normal, {ok, [binary()]},tuple()} |
                     {stop, normal, {error, error_saving},tuple()} |
                     {reply, need_more_shards, wait_Shards, tuple()}.
wait_shards({shard,Shard}, _, S=#state{
                                   ensemble=Ensemble,
                                   shards=Shards0,
                                   threshold=T,
                                   sk=SK,
                                   esk=ESK,
                                   num_shards=N}) ->
  Shards = Shards0 ++ [ binary_to_term(base64:decode(Shard)) ],
  case length(Shards) =:= T of
    true ->
      try
        CurrentOK = shamir:recover(Shards),
        {ok,SK} = cinched_crypto:decrypt(CurrentOK,ESK),
        %% If we got here, key is recovered and we can generate new one.
        NewOK = cinched_crypto:key(),
        {ok, NewESK} = cinched_crypto:encrypt(NewOK,SK),
        NewShards = [
                  base64:encode(term_to_binary(X))
                  || X <- shamir:share(NewOK, T, N)
                 ],
        case cinched_keystore:overwrite(<<"esk">>,Ensemble,NewESK) of
          ok ->
            {stop, normal, {ok, NewShards}, S};
          _ ->
            %% Error saving to ensemble. Start over.
            {stop, normal,{error, error_saving}, S}
        end
      catch
        _:_ ->
          {reply, need_more_shards, wait_shards, S#state{shards=[]}}
      end;
    false ->
      {reply, need_more_shards, wait_shards, S#state{shards=Shards}}
  end.

-spec handle_event(_,atom(),tuple()) -> {next_state, atom(), tuple()}.
handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

-spec handle_sync_event(_,_,atom(),tuple) -> {reply,ok,atom(),tuple()}.
handle_sync_event(_Event, _From, StateName, State) ->
  {reply, ok, StateName, State}.

-spec handle_info(_,atom(),tuple()) -> {next_state,atom(),tuple()}.
handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

-spec terminate(_,_,_) -> ok.
terminate(_Reason, _StateName, _State) ->
  ok.

-spec code_change(_,atom(),tuple,_) -> {ok, atom(),tuple()}.
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.
