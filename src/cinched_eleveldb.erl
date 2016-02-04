%%%-------------------------------------------------------------------
%%% @author Mark Steele <mark@control-alt-del.org>
%%% @copyright (C) 2016, Mark Steele
%%% @doc
%%% Cinched eLevelDB proxy
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
-module(cinched_eleveldb).

-behaviour(gen_server).
-include("cinched.hrl").

-record(state, {
          id :: term(),
          ref :: eleveldb:db_ref(),
          data_root :: string(),
          open_opts = [],
          config = [],
          read_opts = [],
          write_opts = [],
          fold_opts = [{fill_cache, false}]
         }).

%% API
-export([
         start_link/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(non_neg_integer()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Ensemble) ->
  gen_server:start_link(
    {local, list_to_atom("leveldb_" ++ integer_to_list(Ensemble))},
    ?MODULE, [Ensemble], []
   ).

-spec init([non_neg_integer()]) -> {ok, term()}.
init([Ensemble]) ->
  Config =  [
             {compression,true},
             {fadvise_willneed,false},
             {eleveldb_threads,71},
             {verify_compaction,true},
             {verify_checksums,true},
             {block_size_steps,16},
             {block_restart_interval,16},
             {sst_block_size,4096},
             {block_cache_threshold,33554432},
             {use_bloomfilter,true},
             {write_buffer_size_max,62914560},
             {write_buffer_size_min,31457280},
             {create_if_missing,true},
             {limited_developer_mem,false},
             {sync,false},
             {total_leveldb_mem_percent,70},
             {data_root,?PLATFORM_DATA_DIR},
             {delete_threshold,1000},
             {tiered_slow_level,0}
            ],
  {OpenOpts, _BadOpenOpts} = eleveldb:validate_options(open, Config),
  {ReadOpts, _BadReadOpts} = eleveldb:validate_options(read, Config),
  {WriteOpts, _BadWriteOpts} = eleveldb:validate_options(write, Config),
  FoldOpts = lists:keystore(fill_cache, 1, ReadOpts, {fill_cache, false}),
  DataRoot = filename:join(?PLATFORM_DATA_DIR,integer_to_list(Ensemble)),
  State =  #state {
              id = Ensemble,
              data_root = DataRoot,
              open_opts = OpenOpts,
              read_opts = ReadOpts ++ [read_repair],
              write_opts = WriteOpts,
              fold_opts = FoldOpts,
              config = Config
             },
  case open_db(State) of
    {error, _} ->
      exit("Error opening db");
    {ok, NewState} ->
      {ok, NewState}
  end.

-spec handle_call(term(),term(),term()) -> {reply,term(),term()}.
handle_call(backup, _From, State=#state{ref=Ref,data_root=Path}) ->
  ok = eleveldb:close(Ref),
  {ok, BackupPath} = leveldb_snapshot:snapshot(Path),
  {ok, NewState} = open_db(State),
  {reply, {ok, BackupPath}, NewState};
handle_call({remove_backup, Path}, _From, State) ->
  case re:run(Path,"^/var/lib/cinched/\\d+/snapshot-.*$",[{capture,none}]) of
    match ->
      ok = ec_file:remove(Path,[recursive]),
      {reply, ok, State};
    _ ->
      {reply, error, State}
  end;

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

-spec handle_cast(term(),term()) -> {noreply, term()}.
handle_cast({get, Key, From},State=#state{read_opts=ReadOpts,ref=Ref}) ->
  Reply = case eleveldb:get(Ref,Key,ReadOpts) of
            {ok, Value} ->
              binary_to_term(Value);
            _ ->
              notfound
          end,
  riak_ensemble_backend:reply(From, Reply),
  {noreply, State};

handle_cast({put, Key, Obj, From}, State=#state{write_opts=WriteOps, ref=Ref}) ->
  Reply = case eleveldb:write(Ref,[{put,Key,term_to_binary(Obj)}],WriteOps) of
            ok ->
              Obj;
            _ ->
              error
          end,
  riak_ensemble_backend:reply(From, Reply),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

-spec handle_info(_,_) -> {noreply, term()}.
handle_info(_, State) ->
  {noreply, State}.

-spec terminate(_, _) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change(_,_,_) -> {ok, any()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec open_db(any()) -> {ok, any()} | {error, any()}.
open_db(State) ->
  RetriesLeft = 30,
  open_db(State, max(1, RetriesLeft), undefined).

-spec open_db(any(),integer(),any()) -> {ok, any()} | {error, any()}.
open_db(_State0, 0, LastError) ->
  {error, LastError};
open_db(State0, RetriesLeft, _) ->
  case eleveldb:open(State0#state.data_root, State0#state.open_opts) of
    {ok, Ref} ->
      {ok, State0#state { ref = Ref }};
    %% Check specifically for lock error, this can be caused if
    %% a crashed vnode takes some time to flush leveldb information
    %% out to disk.  The process is gone, but the NIF resource cleanup
    %% may not have completed.
    {error, {db_open, OpenErr}=Reason} ->
      case lists:prefix("IO error: lock ", OpenErr) of
        true ->
          SleepFor = 2000,
          timer:sleep(SleepFor),
          open_db(State0, RetriesLeft - 1, Reason);
        false ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.
