%%%-------------------------------------------------------------------
%%% @author Mark Steele <mark@control-alt-del.org>
%%% @copyright (C) 2016, Mark Steele
%%% @doc
%%% Cinched console, to be consumed by the RPC service
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
-module(cinched_console).

-export([
         status/1,
         stop/1,
         stats/1,
         rotate_audit_log/1,
         check_audit_log/2,
         view_audit_log/2,
         list_audit_logs/1,
         init_cluster/1,
         send_key_shard/2,
         backup/1,
         rotate_ok/1,
         rotate_ok_shard/2
         ]).

-define(TIMEOUT,100000).
-define(CHECK_STATUS,fun(State,Fun) ->
                         case cinched:status() of
                           State ->
                             Fun();
                           _ ->
                             io:format("Error running command, check cluster status~n",[]),
                             {error,invalid_status}
                         end
                     end).

-spec init_cluster(list()) -> ok | error.
init_cluster(_User) ->
  ?CHECK_STATUS(
     waiting_init,
     fun() ->
         case gen_fsm:sync_send_event(cinched_load_esk_fsm,init,?TIMEOUT) of
           {ok, Shards} ->
             [ ok = io:format("~s\n",[X]) || X <- Shards],
             ok;
           _ ->
             io:format("Error initializing the cluster, please retry the cluster initialization~n",[]),
             error
         end
     end
    ).

-spec send_key_shard(list(),list()) -> ok.
send_key_shard(_User,Shard) ->
  ?CHECK_STATUS(
     waiting_shards,
     fun() ->
         case gen_fsm:sync_send_event(cinched_load_esk_fsm,{shard,Shard},?TIMEOUT) of
           key_recovered ->
             io:format("~nKey recovered from shards~n",[]);
           need_more_shards ->
             io:format("Need more shards~n",[])
         end
     end
    ).

-spec status(string()) -> 'ok'.
status(User) ->
  catch cinched_log:log([{op,status}, {user,list_to_binary(User)}]),
  io:format("Status: ~p~n",[cinched:status()]),
  ok.

-spec stats(string()) -> 'ok'.
stats(User) ->
  ?CHECK_STATUS(
     started,
     fun() ->
         cinched_log:log([{op,stats}, {user,list_to_binary(User)}]),
         case cinched:stats() of
           {ok, Stats} ->
             lists:foreach(
               fun({X,V}) ->
                   io:format("~p: ~p~n",[format_stat(X),V])
               end,
               Stats);
           _ ->
             io:format("Error retrieving stats, is the service configured?~n",[])
         end
     end
    ).

-spec stop(list()) -> ok.
stop(User) ->
  catch cinched_log:log([{op,stop}, {user,list_to_binary(User)}]),
  init:stop(),
  erlang:halt().

-spec format_stat([atom()]) -> string().
format_stat(X) ->
  string:join([format_thing(A) || A <- X ],".").

-spec format_thing(atom() | integer()) -> list().
format_thing(X) when is_atom(X) ->
  atom_to_list(X);
format_thing(X) when is_integer(X) ->
  integer_to_list(X).

-spec rotate_audit_log(list()) -> ok.
rotate_audit_log(User) when
    is_list(User),
    length(User) > 0 ->
  ?CHECK_STATUS(
     started,
     fun() ->
         %% Send audit event before rotating
         cinched_log:log([{op,rotate}, {user,list_to_binary(User)}]),
         ok = cinched_log:rotate(),
         io:format("Status: ok~n",[]),
         %% And after for good measure.
         cinched_log:log([{op,rotate}, {user,User}])
     end
    ).

-spec check_audit_log(list(),list()) -> ok.
check_audit_log(User,Log) when
    is_list(Log),
    is_list(User),
    length(User) > 0,
    length(Log) > 0 ->
  ?CHECK_STATUS(
     started,
     fun() ->
         cinched_log:log([{op,check_log},{user,list_to_binary(User)},{log,list_to_binary(Log)}]),
         case cinched_log_viewer:check_log(Log) of
           ok ->
             io:format("Status: ok~n");
           _ ->
             io:format("Status: error~n")
         end
     end
    ).

-spec view_audit_log(list(),list()) -> ok.
view_audit_log(User,Log) when
    is_list(Log),
    is_list(User),
    length(User) > 0,
    length(Log) > 0 ->
  ?CHECK_STATUS(
     started,
     fun() ->
         {ok, SK} = application:get_env(cinched,sk),
         cinched_log:log([{op,view_log},{user,list_to_binary(User)},{log,list_to_binary(Log)}]),
         qdate:set_timezone("GMT"),
         To = self(),
         spawn(fun() ->
                   ok = cinched_log_viewer:view_log(Log,cinched:generate_hash(SK),To)
               end),
         receive_log_data()
     end
    ).

%% TODO: Should add timeout here.
-spec receive_log_data() -> ok.
receive_log_data() ->
  receive
    {ok,{data,Data},{timestamp,T}} ->
      io:format("~s~n",[jiffy:encode({[
                                       {timestamp,list_to_binary(qdate:to_string("Y-m-d H:i:s P",T))}|
                                       binary_to_term(Data)
                                       ]})]),
      receive_log_data();
    eof ->
      io:format("Log dump complete~n");
    _ ->
      receive_log_data()
  end.

-spec list_audit_logs(list()) -> ok.
list_audit_logs(User) when
    is_list(User),
    length(User) > 0 ->
  ?CHECK_STATUS(
     started,
     fun() ->
         cinched_log:log([{op,list_logs},{user,list_to_binary(User)}]),
         lists:foreach(
           fun(X) ->
               io:format("~p~n",[X])
           end,
           cinched_log_viewer:list_logs()
          )
     end
    ).

-spec backup(list()) -> ok.
backup(User) ->
  ?CHECK_STATUS(
     started,
     fun() ->
         cinched_log:log([{op,backup},{user,list_to_binary(User)}]),
         ok = cinched:backup(),
         io:format("Backup succeeded~n",[])
     end
    ).

-spec rotate_ok(list()) -> ok.
rotate_ok(User) ->
  ?CHECK_STATUS(
     started,
     fun() ->
         cinched_log:log([{op,rotate_ok}, {user,list_to_binary(User)}]),
         case is_pid(whereis(cinched_ok_rotate_fsm)) of
           true ->
             io:format("Operator key initiated already.~n",[]);
           false ->
             {ok,_} = cinched_ok_rotate_fsm:start_link(),
             io:format("Initiating operator key rotation.~nPlease input shard keys using the 'cinched-admin rotate-operator-key-send-shard' command~n",[])
         end
     end
    ).

-spec rotate_ok_shard(list(),list()) -> ok.
rotate_ok_shard(User,Shard) ->
  ?CHECK_STATUS(
     started,
     fun() ->
         cinched_log:log([{op,rotate_ok_send_shard}, {user,list_to_binary(User)}]),
         case gen_fsm:sync_send_event(cinched_ok_rotate_fsm,{shard,Shard},?TIMEOUT) of
           {ok, Shards} ->
             io:format("Service key successfully re-encrypted with new Operator Key. New key shards:~n",[]),
             [ ok = io:format("~s\n",[X]) || X <- Shards];
           need_more_shards ->
             io:format("Need more shards~n",[]);
           _ ->
             io:format("Error received, please try again~n",[])
         end
     end
    ).
