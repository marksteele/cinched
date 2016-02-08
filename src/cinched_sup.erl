%%%-------------------------------------------------------------------
%%% @author Mark Steele <mark@control-alt-del.org>
%%% @copyright (C) 2016, Mark Steele
%%% @doc
%%% Cinched main supervisor.
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
-module(cinched_sup).

-behaviour(supervisor).
-include("cinched.hrl").
%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================
-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-spec init([]) -> {'ok',{{'one_for_one',5,10},[{_,_,_,_,_,_},...]}}.
init([]) ->

  StartupFSM = {
    cinched_startup_fsm,
    {cinched_startup_fsm, start_link,[]},
    permanent, 20000, worker, [cinched_startup_fsm]
   },

  Ensemble = {
    riak_ensemble_sup,
    {riak_ensemble_sup, start_link,[?PLATFORM_DATA_DIR]},
    permanent, 20000, supervisor, [riak_ensemble_sup]
   },

  EnsembleFSM = {
    cinched_ensemble_fsm,
    {cinched_ensemble_fsm,start_link,[]},
    permanent, 10000, worker, [cinched_ensemble_fsm]
   },

  ESKFSM = {
    cinched_load_esk_fsm,
    {cinched_load_esk_fsm,start_link,[]},
    permanent, 10000, worker, [cinched_load_esk_fsm]
   },

  {ok,
   {
     {one_for_one, 5, 10},
     [
      StartupFSM, Ensemble, EnsembleFSM, ESKFSM
     ]
   }
  }.
