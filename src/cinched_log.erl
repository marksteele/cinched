%%%-------------------------------------------------------------------
%%% @author Mark Steele <mark@control-alt-del.org>
%%% @copyright (C) 2016, Mark Steele
%%% @doc
%%% Cinched audit logging service
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
-module(cinched_log).

-behaviour(gen_server).

-include("cinched.hrl").

-export([
         start_link/0,
         rotate/0,
         log/1
        ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {current_key, current_hash, log, path, hash}).

-spec start_link() -> {ok,pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE,[], []).

-spec log(term()) -> ok.
log(Payload) ->
  gen_server:call(?SERVER,{log, Payload},infinity).

-spec rotate() -> ok.
rotate() ->
  gen_server:call(?SERVER,rotate).

-spec init([]) -> {ok, tuple()}.
init([]) ->
  {ok, SK} = application:get_env(cinched,sk),
  CurrentKey = cinched:generate_hash(SK),
  LogPath = filename:join(?PLATFORM_LOG_DIR,"audit"),
  File = filename:join(LogPath,"audit.log"),
  ok = filelib:ensure_dir(File),
  ok = rename_old_log(LogPath, File),
  {ok, Log} = disk_log:open([{name, audit},{file,File}]),
  timer:apply_interval(?AUDIT_LOG_SYNC_INTERVAL, gen_server, cast, [?SERVER,sync]),
  {ok, #state{
          hash = SK,
          current_key = CurrentKey,
          current_hash = <<>>,
          log = Log,
          path = LogPath}}.

-spec handle_call(rotate,_,tuple()) -> {reply,ok,tuple()} ;
                 (log,_,tuple()) -> {reply,ok,tuple()}.
handle_call(rotate, _From, S=#state{log=Log, path=Path, hash=Hash}) ->
  File = create_filename(Path),
  ok = disk_log:reopen(Log,File),
  CurrentKey = cinched:generate_hash(Hash),
  {reply, ok, S#state{current_key = CurrentKey, current_hash = <<>>}};

handle_call({log, P}, _, S=#state{log=Log, current_key=K,current_hash=H}) ->
  EP = term_to_binary(P,[compressed]),
  {ok, EEP} = nacl:secretbox(EP,K),
  EEEP = term_to_binary(EEP),
  Hash = nacl:hash(<<EEEP/binary, H/binary>>),
  Record = #cinched_log{
              hash = Hash,
              payload = EEEP,
              timestamp = os:timestamp()
             },
  ok = disk_log:log(Log, Record),
  NextKey = cinched:generate_hash(K),
  {reply, ok, S#state{current_key = NextKey, current_hash=Hash}}.

-spec handle_cast(sync,tuple()) -> {noreply, tuple()}.
handle_cast(sync, S=#state{log=Log}) ->
  ok = disk_log:sync(Log),
  {noreply,S}.

-spec handle_info(_,tuple()) -> {noreply,tuple()}.
handle_info(_Info, State) ->
  {noreply, State}.

-spec terminate(_,_) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change(_,tuple,_) -> {ok,tuple()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

-spec create_filename(list()) -> list().
create_filename(Path) ->
  File = Path ++ "/" ++ "audit.log." ++
    integer_to_list(cinched:unix_timestamp()),
  case filelib:is_file(File) of
    true ->
      timer:sleep(1000),
      create_filename(Path);
    false ->
      File
  end.

-spec rename_old_log(list(),list()) -> ok.
rename_old_log(Path,File) ->
  case filelib:is_file(File) of
    true ->
      NewFile = create_filename(Path),
      ok = file:rename(File, NewFile);
    false ->
      ok
  end.
