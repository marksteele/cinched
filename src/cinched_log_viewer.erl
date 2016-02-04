%%%-------------------------------------------------------------------
%%% @author Mark Steele <mark@control-alt-del.org>
%%% @copyright (C) 2016, Mark Steele
%%% @doc
%%% Cinched audit log viewer
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
-module(cinched_log_viewer).

-include("cinched.hrl").

-export([
         check_log/1,
         view_log/3,
         list_logs/0
        ]).

-spec validate_log(list()) -> ok | error.
validate_log(Log) ->
  case re:run(Log,"^" ++ ?PLATFORM_LOG_DIR ++ "/audit/audit\\.log(\\.\\d+)?$",[{capture,none}]) of
    match ->
      case filelib:is_file(Log) of
        true ->
          ok;
        _ ->
          error
      end;
    _->
      error
  end.

-spec check_log(list()) -> ok | error.
check_log(File) ->
  case validate_log(File) of
    ok ->
      {ok, Log} = disk_log:open([{name,audit_read},{file,File},{mode,read_only}]),
      Fun = fun(Terms,Acc) ->
                check_hashes(Terms,Acc)
            end,
      Response = process_log(Log,Fun,init,<<>>),
      disk_log:close(Log),
      Response;
    _ ->
      error
  end.

-spec view_log(list(), binary(),pid()) -> ok | error.
view_log(File,Hash,To) ->
  case validate_log(File) of
    ok ->
      {ok, Log} = disk_log:open([{name,audit_read},{file,File},{mode,read_only}]),
      Fun = fun(Terms,Acc) ->
                decode_data(Terms,Acc,To)
            end,
      Response = process_log(Log,Fun,init,{Hash,<<>>}),
      disk_log:close(Log),
      To ! eof,
      Response;
    _ ->
      To ! eof,
      ok
  end.

-spec process_log(any(),fun(),term(),{binary(),binary()} | <<>>) -> ok | error.
process_log(Log,Fun,init,Init) ->
  process_log(Log,Fun,start,Init);
process_log(Log,Fun,Cont,Acc) ->
  try
    case disk_log:chunk(Log, Cont) of
      {error, _} ->
        throw(error);
      {Cont2, Terms} ->
        Acc2 = Fun(Terms,Acc),
        process_log(Log,Fun,Cont2, Acc2);
      {Cont2, Terms, _} ->
        Acc2 = Fun(Terms,Acc),
        process_log(Log,Fun,Cont2,Acc2);
      eof ->
        ok
    end
  catch
    _:_ ->
      error
  end.

-spec check_hashes(term(),term()) -> any().
check_hashes(Terms,Acc) ->
  lists:foldl(
    fun(#cinched_log{payload=P,hash=H},HC) ->
        H = nacl:hash(<<P/binary, HC/binary>>)
    end,
    Acc,
    Terms).

-spec decode_data(term(),term(),pid()) -> any().
decode_data(Terms,Acc,To) ->
  lists:foldl(
    fun(#cinched_log{payload=P,hash=H,timestamp=T},{K,HC}) ->
        H = nacl:hash(<<P/binary, HC/binary>>),
        case nacl:secretbox_open(binary_to_term(P),K) of
          {error, _} ->
            To ! error;
           {ok, Data} ->
            To ! {ok, {data,Data},{timestamp,T}}
        end,
        {cinched:generate_hash(K), H}
    end,
    Acc,
    Terms).

-spec list_logs() -> list().
list_logs() ->
  filelib:fold_files(
    filename:join(?PLATFORM_LOG_DIR,"audit"),
    "^audit.log(?:\.\d+)?",
    false,
    fun(X, Acc) ->
        [{X, filelib:file_size(X)}] ++ Acc
    end,
    []
   ).
