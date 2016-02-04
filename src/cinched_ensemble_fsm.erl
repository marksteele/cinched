%%%-------------------------------------------------------------------
%%% @author Mark Steele <mark@control-alt-del.org>
%%% @copyright (C) 2016, Mark Steele
%%% @doc
%%% Cinched ensemble startup fininte state machine.
%%% This FSM tries to bootstrap the ensemble startup
%%% sequence.
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
-module(cinched_ensemble_fsm).

-behaviour(gen_fsm).

-export([start_link/0]).

-export([
         init/1,
         check_ensemble_enabled/2,
         wait_for_cluster/2,
         wait_for_ensembles/2,
         create_missing_ensembles/2,
         bootstrap_ensemble/2,
         check_cluster/2,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4
        ]).

-define(SERVER, ?MODULE).

-record(state, {ring,rootleader,nodes}).

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec init([]) -> {ok, check_ensemble_enabled, tuple(),0}.
init([]) ->
  Ring = cinched:ring(),
  {ok, Nodes} = application:get_env(cinched,nodes),
  {ok, check_ensemble_enabled, #state{ring=Ring,nodes=Nodes}, 0}.

-spec check_ensemble_enabled(timeout,tuple()) ->
                                {next_state,check_cluster,tuple(),0} |
                                {next_state,wait_for_ensembles,tuple(),10000}.
check_ensemble_enabled(timeout,S) ->
  case riak_ensemble_manager:enabled() of
    true ->
      cinched:wait_ensemble_stable(root),
      RootLeader = riak_ensemble_manager:rleader_pid(),
      case is_pid(RootLeader) andalso node(RootLeader) == node() of
        true ->
          {next_state, check_cluster, S#state{rootleader=RootLeader}, 0};
        false ->
          {next_state, wait_for_ensembles, S, 10000}
      end;
    false ->
      {next_state, wait_for_cluster, S, 0}
  end.

-spec wait_for_ensembles(timeout,tuple()) ->
                            {stop,normal,tuple()} |
                            {next_state,wait_for_ensembles,tuple(),1000}.
wait_for_ensembles(timeout, S=#state{ring=Ring}) ->
  {ok, KnownEnsembles} = riak_ensemble_manager:known_ensembles(),
  CurrentEnsembles = orddict:fetch_keys(KnownEnsembles),
  MissingEnsembles = proplists:get_keys(Ring) -- CurrentEnsembles,
  case MissingEnsembles of
    [] ->
      gen_fsm:send_event(cinched_startup_fsm, ensemble_up),
      {stop, normal, S};
    _ ->
      {next_state, wait_for_ensembles, S, 1000}
  end.

-spec check_cluster(timeout,tuple()) ->
                       {next_state, create_missing_ensembles,tuple(),0}.
check_cluster(timeout, S=#state{
                      nodes=Nodes,
                      rootleader=RootLeader
                     }) ->
  Cluster = riak_ensemble_manager:cluster(),
  [ begin
      case catch rpc:call(X, erlang, whereis, [riak_ensemble_manager]) of
        Pid when is_pid(Pid) ->
          riak_ensemble_manager:join(node(), X);
        _ ->
          ok
      end
    end
    || X <- Nodes -- Cluster],

  RootMembers = proplists:get_all_values(
                  root,riak_ensemble_manager:get_members(root)
                 ),
  [
   riak_ensemble_peer:update_members(RootLeader, [{add,{root,X}}], 10000) ||
    X <- Nodes -- RootMembers
  ],
  {next_state, create_missing_ensembles, S, 0}.

-spec create_missing_ensembles(timeout,tuple()) ->
                                  {stop, normal,tuple()} |
                                  {next_state,create_missing_ensembles,tuple(),1000}.
create_missing_ensembles(timeout, S=#state{ring=Ring}) ->
  {ok, KnownEnsembles} = riak_ensemble_manager:known_ensembles(),
  CurrentEnsembles = orddict:fetch_keys(KnownEnsembles),
  ExpectedEnsembles = proplists:get_keys(Ring),
  %% +1 for root ensemble...
  case (length(ExpectedEnsembles) +1) =/= length(CurrentEnsembles) of
    true ->
      MissingEnsembles = ExpectedEnsembles -- CurrentEnsembles,
      Created = lists:foldl(
                  fun(X,Acc) ->
                      Peers = proplists:get_value(X,Ring),
                      case catch riak_ensemble_manager:create_ensemble(
                                   X,
                                   undefined,
                                   [{X,Node} || Node <- Peers],
                                   cinched_ensemble_backend,
                                   []
                                  ) of
                        ok ->
                          Acc ++ [X];
                        _ ->
                          Acc
                      end
                  end,
                  [],
                  MissingEnsembles),
      case length(Created) =:= length(MissingEnsembles) of
        true ->
          gen_fsm:send_event(cinched_startup_fsm, ensemble_up),
          {stop, normal, S};
        _ ->
          {next_state, create_missing_ensembles, S, 1000}
      end;
    false ->
      gen_fsm:send_event(cinched_startup_fsm, ensemble_up),
      {stop, normal, S}
  end.

-spec wait_for_cluster(timeout,tuple()) ->
                          {next_state,bootstrap_ensemble,tuple(),0} |
                          {check_ensemble_enabled,tuple(),1000}.
wait_for_cluster(timeout, S=#state{nodes=Nodes}) ->
  {Enabled, HaveErrors} = check_ensemble_nodes(Nodes),
  case {Enabled, HaveErrors} of
    {[], false} ->
      {next_state, bootstrap_ensemble, S, 0};
    _ ->
      {next_state, check_ensemble_enabled, S, 1000}
  end.

-spec bootstrap_ensemble(timeout,tuple())->
                            {next_state, check_ensemble_enabled, tuple(), 5000 | 10000}.
bootstrap_ensemble(_,S=#state{nodes=Nodes}) ->
  case global:trans({ensemble_bootstrap,self()},
                    fun() ->
                        try
                          riak_ensemble_manager:enable()
                        catch
                          _:_ ->
                            error
                        end
                    end,
                    Nodes,0) of
    ok ->
      {next_state, check_ensemble_enabled, S, 5000};
    _ ->
      {next_state, check_ensemble_enabled, S, 10000}
  end.

-spec handle_event(_,atom(),tuple()) -> {next_state,atom(),tuple()}.
handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

-spec handle_sync_event(status,_,atom(),tuple()) -> {reply,atom(),atom(),tuple(),0}.
handle_sync_event(status, _, StateName, S) ->
  {reply, StateName, StateName, S, 0}.

-spec handle_info(_,atom(),tuple()) -> {next_state,atom(),tuple()}.
handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

-spec terminate(_,_,_) -> ok.
terminate(_Reason, _StateName, _State) ->
  ok.

-spec code_change(_,atom(),tuple(),_) -> {ok,atom(),tuple()}.
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

-spec check_ensemble_nodes([node()]) -> {[node()], boolean()}.
check_ensemble_nodes(Nodes) ->
  Status = lists:foldl(
             fun(X,Acc) ->
                 case catch rpc:call(X,riak_ensemble_manager,enabled,[]) of
                   true ->
                     Acc ++ [{X, true}];
                   false ->
                     Acc ++ [{X, false}];
                   _ ->
                     Acc ++ [{X, error}]
                 end
             end,
             [],
             Nodes),
  HaveErrors = [ X || {X, Y} <- Status, Y =:= error ] =/= [],
  Enabled = [ X || {X, Y} <- Status, Y =:= true ],
  {Enabled, HaveErrors}.
