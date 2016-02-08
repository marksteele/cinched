%%%-------------------------------------------------------------------
%%% @author Mark Steele <mark@control-alt-del.org>
%%% @copyright (C) 2016, Mark Steele
%%% @doc
%%% Cinched application
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
-module(cinched_app).

-behaviour(application).

-include("cinched.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
-spec start(_,_) -> {'error',_} | {'ok',pid()}.
start(_StartType, _StartArgs) ->
  case os:cmd("/usr/sbin/getenforce") =:= "Enforcing\n" andalso
    os:cmd("/usr/sbin/semodule -l | /bin/grep -e " ++
           "'\\(cinched[[:space:]]1.0\\|localuser[[:space:]]0.1\\)'")
    =:= "cinched\t1.0\t\nlocaluser\t0.1\t\n" of
    true ->
      %% Set process limits
      %% Make sure we have enough FDs
      {ok, _} = perc:setrlimit(
                  rlimit_nofile,
                  <<?OPEN_FILE_LIMIT:8/native-unsigned-integer-unit:8,
                    ?OPEN_FILE_LIMIT:8/native-unsigned-integer-unit:8>>
                 ),

      %% No core files!
      {ok, _} = perc:setrlimit(
                  rlimit_core,
                  <<0:8/native-unsigned-integer-unit:8,
                    0:8/native-unsigned-integer-unit:8>>
                 ),

      %% Set the erlang cookie
      {ok,BinCookie} = file:read_file("/var/lib/cinched/erlang.cookie"),
      erlang:set_cookie(node(),binary_to_atom(BinCookie,latin1)),


      %% Drop privileges.
      Pwd = passwderl:getpwnam(?CINCHED_USER),
      ok = sanitize_env(Pwd),
      ok = passwderl:setegid(Pwd#passwderl_pwd.gid),
      ok = passwderl:seteuid(Pwd#passwderl_pwd.uid),

      application:set_env(foldrerl,ca,?CACERTFILE),
      application:set_env(foldrerl,cert,?CERTFILE),
      application:set_env(foldrerl,key,?KEYFILE),
      {ok,IP} = application:get_env(cinched,ip),
      {ok,BACKUP_PORT} = application:get_env(cinched,backup_port),
      application:set_env(foldrerl,address,{IP,BACKUP_PORT}),

      %% TODO
      %% Deps started after dropping privileges. Should move
      %% these to the main supervisor to bubble up failures since
      %% we can't drop privileges before the main app starts.
      Deps = [
              ssl,
              ranch,
              cowboy,
              cache,
              exometer_core,
              poolboy,
              foldrerl,
              leveldb_manager
             ],
      [ application:ensure_all_started(X, permanent) || X <- Deps ],
      %% Kick off startup sequence. Should move everything into main sup.
      cinched_sup:start_link();
    _ ->
      {error,"Must be run in enforcing SELinux with policy loaded"}
  end.

-spec stop(_) -> 'ok'.
stop(_State) ->
  ok.


-spec sanitize_env(tuple()) -> ok.
sanitize_env(Pwd) ->
  {Uid, Gid} = {Pwd#passwderl_pwd.uid,Pwd#passwderl_pwd.gid},

  file:change_owner(filename:join(?PLATFORM_BIN_DIR,"cinched"),0,0),
  file:change_mode(filename:join(?PLATFORM_BIN_DIR,"cinched"),8#00500),

  filelib:ensure_dir(filename:join([?PLATFORM_LOG_DIR,"audit","dummy"])),
  filelib:ensure_dir(filename:join(?PLATFORM_DATA_DIR,"dummy")),
  file:change_owner(?PLATFORM_LOG_DIR,Uid,Gid),
  file:change_owner(?PLATFORM_DATA_DIR,Uid,Gid),
  file:change_owner(filename:join(?PLATFORM_LOG_DIR,"audit"),Uid,Gid),

  filelib:fold_files(?PLATFORM_LOG_DIR,
                     ".*",
                     true,
                     fun(X,Acc) ->
                         file:change_owner(X,Uid,Gid),
                         file:change_mode(X,8#00700),
                         Acc
                     end,
                     []),
  filelib:fold_files(?PLATFORM_DATA_DIR,
                     ".*",
                     true,
                     fun(X,Acc) ->
                         file:change_owner(X,Uid,Gid),
                         file:change_mode(X,8#00700),
                         Acc
                     end,
                     []),

  filelib:fold_files(?PLATFORM_LIB_DIR,
                     ".*",
                     true,
                     fun(X,Acc) ->
                         file:change_owner(X,Uid,Gid),
                         file:change_mode(X,8#00500),
                         Acc
                     end,
                     []),
  filelib:fold_files(?PLATFORM_ETC_DIR,
                     ".*",
                     true,
                     fun(X,Acc) ->
                         file:change_owner(X,Uid,Gid),
                         file:change_mode(X,8#00500),
                         Acc
                     end,
                     []),
  ok.
