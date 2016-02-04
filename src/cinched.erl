%%%-------------------------------------------------------------------
%%% @author Mark Steele <mark@control-alt-del.org>
%%% @copyright (C) 2016, Mark Steele
%%% @doc
%%% Cinched API
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
-module(cinched).
-include("cinched.hrl").
-include_lib("public_key/include/public_key.hrl").
-define(BACKUP_TIMEOUT,600000).

-export([
         status/0,
         stats/0,
         cowboy_stats/1,
         unix_timestamp/0,
         generate_hash/1,
         mk_reqid/0,
         ring/0,
         get_ensemble/2,
         wait_for_reqid/2,
         hash/1,
         ring_increment/1,
         generate_data_key/0,
         backup/0,
         wait_ensemble_stable/1,
         get_cert_info_from_socket/1
        ]).

%% @doc Returns status of dependent services.
-spec status() -> started | waiting_init | waiting_shards |
                  waiting_ensemble | {error,service_not_started}.
status() ->
  case whereis(cinched_startup_fsm) of
    undefined ->
      {error, service_not_started};
    _ ->
      gen_fsm:sync_send_all_state_event(cinched_startup_fsm,status)
  end.

%% @doc Return exometer stats
-spec stats() -> {'ok',_} | error.
stats() ->
  %% Don't try to grab stats if not online...
  case status() of
    started ->
      {ok, ExoMetrics} = exometer_report:list_metrics(),
      {ok, lists:foldl(
             fun({Name,DP,_,Status}, Acc) ->
                 case Status of
                   enabled ->
                     Type = proplists:get_value(type, exometer:info(Name)),
                     Acc ++ get_datapoints(Type, Name, DP);
                   _ ->
                     Acc
                 end
             end,
             [],
             ExoMetrics
            )};
    _ ->
      error
  end.

%% Private API

%% @doc Get datapoints for provided metrics.
-spec get_datapoints('counter' | 'function' | 'guage' | 'histogram',list(),list()) -> [any()].
get_datapoints(Type, Name, _DP) when Type =:= counter;
                                    Type =:= guage ->
  {ok, [{value,X},_]} = exometer:get_value(Name),
  [{Name, X}];
get_datapoints(Type, Name, DP) when Type =:= function; Type =:= histogram ->
  lists:foldl(
    fun(Item,Acc) ->
        {ok, [{_,V}]} = exometer:get_value(Name,Item),
        Acc ++ [{Name ++ [Item],V}]
    end,
    [],
    DP).

%% @doc Retrieve number of active connections from cowboy. This may be a hack,
%%      wish coyboy would implement some statistics.
-spec cowboy_stats('active_connections') -> non_neg_integer().
cowboy_stats(active_connections) ->
  try
    [{{ranch_listener_sup,https},Pid,_,_},_] =  supervisor:which_children(ranch_sup),
    [_,{ranch_conns_sup,ConnPid,_,_}] = supervisor:which_children(Pid),
    ranch_conns_sup:active_connections(ConnPid)
  catch
    _:_ ->
      0
  end.

%% @doc Return a unix timestamp. Just because.
-spec unix_timestamp() -> non_neg_integer().
unix_timestamp() ->
  {Mega, Secs, _} = now(),
  Mega*1000000 + Secs.

%% @doc Given a binary input, generate a hash that is appropriate
%%      to use as a key for encryption. Uses simple stretching
%%      if the hash function returns a value that's too small.
-spec generate_hash(binary() | nonempty_string()) -> binary().
generate_hash(Input) when is_list(Input) ->
  generate_hash(iolist_to_binary(Input));
generate_hash(Input) when is_binary(Input) ->
  Size = nacl_nif:secretbox_KEYBYTES(),
  generate_hash(Input, Size, <<>>).

-spec generate_hash(binary(),non_neg_integer(),binary()) -> binary().
generate_hash(_, Size, Acc) when size(Acc) >= Size->
  <<Hash:Size/binary, _/binary>> = Acc,
  Hash;
generate_hash(Input, Size, Acc) ->
  More = nacl:hash(<<Acc/binary, Input/binary>>),
  generate_hash(Input, Size,  <<Acc/binary, More/binary>>).


%% @doc Wait for a specific message
-spec wait_for_reqid(integer(),non_neg_integer()) -> ok | {ok, term()} | error | {error, timeout}.
wait_for_reqid(ReqId, Timeout) ->
  receive
    {ReqId, ok} ->
      ok;
    {ReqId, ok, Response} ->
      {ok, Response};
    {ReqId, error} ->
      error
  after Timeout ->
      {error, timeout}
  end.

-spec hash(term()) -> binary().
hash(Obj) ->
  crypto:hash(sha, term_to_binary(Obj)).

-spec mk_reqid() -> integer().
mk_reqid() ->
  erlang:phash2(erlang:now()).

-spec partitions(1..1024) -> list().
partitions(NumPartitions) when NumPartitions < 1025 ->
  Inc = ring_increment(NumPartitions),
  [X || X <- lists:seq(0,(?RINGTOP-1),Inc)].

-spec ring_increment(1..1024) -> integer().
ring_increment(NumPartitions) ->
  ?RINGTOP div NumPartitions.

-spec pick_nodes(list(),list(),pos_integer(),list()) -> list().
pick_nodes([], _Nodes, _N, Acc) ->
  Acc;
pick_nodes([Idx|Partitions],Nodes,N,Acc) ->
  PartitionNodes = lists:sublist(Nodes,N),
  pick_nodes(
    Partitions,
    (Nodes -- PartitionNodes) ++ PartitionNodes,
    N,
    Acc ++ [{Idx, PartitionNodes}]
   ).

-spec pick_nodes(list(),list(),pos_integer()) -> list().
pick_nodes(Partitions, Nodes,N) ->
  pick_nodes(Partitions, Nodes, N, []).

-spec ring() -> list().
ring() ->
  {ok, Nodes} = application:get_env(cinched,nodes),
  {ok, Partitions} = application:get_env(cinched,ring_size),
  {ok, N} = application:get_env(cinched, n_val),
  pick_nodes(partitions(Partitions),Nodes,N).

-spec responsible_index(binary(),1..1024) -> non_neg_integer().
responsible_index(<<HashKey:160/integer>>, Size) ->
   Inc = ring_increment(Size),
   (((HashKey div Inc) + 1) rem Size) * Inc.

-spec get_ensemble(term(),1..1024) -> non_neg_integer().
get_ensemble(Key, NumPartitions) ->
  responsible_index(hash(Key), NumPartitions).

-spec generate_data_key() -> {ok, tuple()} | {error,term()}.
generate_data_key() ->
  {Time, Value} = timer:tc(
                    fun() ->
                        poolboy:transaction(
                          pb,
                          fun(Worker) ->
                              gen_server:call(Worker,data_key)
                          end
                         )
                    end),
  exometer:update([crypto_worker,data_key,time],Time),
  Value.

-spec backup() -> ok.
backup() ->
  {ok, Nodes} = application:get_env(cinched, nodes),
  Ring = cinched:ring(),
  BackupFolder = filename:join([?PLATFORM_DATA_DIR,"backups"]),
  ok = case filelib:is_dir(BackupFolder) of
    true ->
      ec_file:remove(BackupFolder,[recursive]);
    _ ->
      ok
  end,

  ok = filelib:ensure_dir(filename:join(BackupFolder,"dummy")),
  LocalEnsembles = local_ensembles(Ring),
  RemoteEnsembles = remote_ensembles(Ring,LocalEnsembles),
  ok = backup_local_ensembles(LocalEnsembles,BackupFolder),
  ok = backup_remote_ensembles(RemoteEnsembles,BackupFolder,Nodes),
  ok = backup_root_ensemble(?PLATFORM_DATA_DIR,BackupFolder),
  ok.

-spec get_leveldb_server(integer()) -> atom().
get_leveldb_server(Ensemble) ->
  list_to_atom("leveldb_" ++ integer_to_list(Ensemble)).

-spec backup_local_ensembles(list(),list()) -> ok.
backup_local_ensembles(LocalEnsembles,BackupFolder) ->
  [
   begin
     Server = get_leveldb_server(Ensemble),
     {ok, EnsembleBackupPath} = gen_server:call(Server,backup,?BACKUP_TIMEOUT),
     ok = file:rename(
            EnsembleBackupPath,
            filename:join(BackupFolder,integer_to_list(Ensemble))
           )
   end
   || {Ensemble,_} <- LocalEnsembles],
  ok.

-spec local_ensembles(list()) -> list().
local_ensembles(Ring) ->
  lists:foldl(
    fun({Ensemble,Nodes},Acc) ->
        case lists:member(node(),Nodes) of
          true ->
            Acc ++ [{Ensemble,Nodes}];
          _ ->
            Acc
        end
    end,
    [],
    Ring).

-spec remote_ensembles(list(), list()) -> list().
remote_ensembles(Ring,LocalEnsembles) ->
  Ring -- LocalEnsembles.

-spec filter_offline_nodes(list(),list()) -> list().
filter_offline_nodes(Ensembles,ConfiguredNodes) ->
  OfflineNodes = nodes() -- ConfiguredNodes,
  lists:foldl(
    fun({Ensemble,Nodes},Acc) ->
        Online = Nodes -- OfflineNodes,
        Acc ++ [{Ensemble,Online}]
    end,
    [],
    Ensembles).

-spec ensure_ensemble_online(list()) -> ok | error.
ensure_ensemble_online(Ensembles) ->
  case lists:foldl(
         fun({Ensemble,Nodes},Acc) ->
             case Nodes =:= [] of
               true ->
                 Acc ++ [Ensemble];
               _ ->
                 Acc
             end
         end,
         [],
         Ensembles) of
    [] ->
      ok;
    _ ->
      error
  end.

-spec backup_remote_ensembles(list(),list(),list()) -> ok.
backup_remote_ensembles(RemoteEnsembles,BackupFolder,ConfiguredNodes) ->
  Ensembles = filter_offline_nodes(RemoteEnsembles,ConfiguredNodes),
  ok = ensure_ensemble_online(Ensembles),
  [
   begin
     EnsembleString = integer_to_list(Ensemble),
     Server = get_leveldb_server(Ensemble),
     {ok,RemotePath} = gen_server:call({Server,Node},backup,?BACKUP_TIMEOUT),
     ok = foldrerl:retrieve_folder(
            Node,
            RemotePath,
            filename:join(BackupFolder,EnsembleString)
           ),
     ok = gen_server:call({Server,Node},{remove_backup,RemotePath},?BACKUP_TIMEOUT)
   end || {Ensemble,[Node|_]} <- RemoteEnsembles],
  ok.

-spec backup_root_ensemble(list(),list()) -> ok.
backup_root_ensemble(Path,BackupFolder) ->
  RootPath = filename:join(Path,"ensembles"),
  filelib:fold_files(
    RootPath,
    ".*",
    true,
    fun(X,_) ->
        NewPath = re:replace(X,Path,BackupFolder,[{return,list}]),
        filelib:ensure_dir(NewPath),
        {ok,_} = file:copy(X,NewPath)
    end,
    []),
  ok.

-spec wait_ensemble_stable(non_neg_integer() | root) -> ok.
wait_ensemble_stable(Ensemble) ->
  case check_stable(Ensemble) of
    true ->
      ok;
    false ->
      wait_ensemble_stable(Ensemble)
  end.

-spec check_stable(non_neg_integer() | root) -> true | false.
check_stable(Ensemble) ->
  case riak_ensemble_manager:check_quorum(Ensemble, 1000) of
    true ->
      case riak_ensemble_peer:stable_views(Ensemble, 1000) of
        {ok, true} ->
          true;
        _ ->
          false
      end;
    false ->
      false
  end.

-spec get_cert_info_from_socket({'sslsocket',_,pid() | {port(),_}}) -> 'error' | {'ok',{binary(),_}}.
get_cert_info_from_socket(Socket) ->
  case ssl:peercert(Socket) of
    {error, no_peercert} ->
      error;
    {ok, Cert} ->
      {ok, {Serial,_}} = public_key:pkix_issuer_id(Cert, self),
      CN = get_common_name(public_key:pkix_decode_cert(Cert,otp)),
      {ok,{integer_to_binary(Serial),CN}}
  end.

-spec get_common_name(#'OTPCertificate'{tbsCertificate::#'OTPTBSCertificate'{subject::{_,_}}}) -> any().
get_common_name(OTPCert) ->
  {rdnSequence, Subject} = OTPCert#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.subject,
  case [Attribute#'AttributeTypeAndValue'.value ||
         [Attribute] <- Subject,
         Attribute#'AttributeTypeAndValue'.type == ?'id-at-commonName'] of
    [Att] ->
      case Att of
        {teletexString, Str} ->
          list_to_binary(Str);
        {printableString, Str} ->
          list_to_binary(Str);
        {utf8String, Bin} ->
          Bin
      end;
    _ ->
      unknown
  end.
