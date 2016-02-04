%%%-------------------------------------------------------------------
%%% @author Mark Steele <mark@control-alt-del.org>
%%% @copyright (C) 2016, Mark Steele
%%% @doc
%%% Cinched RPC interface, consumed by the bash script.
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
-module(cinched_rpc).

%% API
-export([start/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec start() -> ok.
start() ->
  error_logger:tty(false),
  %% Set the erlang cookie
  {ok,BinCookie} = file:read_file("/var/lib/cinched/erlang.cookie"),
  erlang:set_cookie(node(),binary_to_atom(BinCookie,latin1)),

  Arguments = init:get_plain_arguments(),
  OptSpec = [
             {target,$t,"target",atom,"Node"},
             {module, $m, "module",atom,"Module name"},
             {function,$f,"function",atom,"Function name"},
             {user,$u,"user",string,"User"},
             {arguments,$a,"arg",{string,undefined},"Function arguments"}
            ],
  case getopt:parse(OptSpec,Arguments) of
    {ok,{Args,_Junk}} ->
      User = proplists:get_value(user,Args),
      RPCArgs = case proplists:get_value(arguments,Args) of
                  undefined ->
                    [User];
                  Value when is_list(Value) ->
                    [User,Value]
                end,
      Target = proplists:get_value(target,Args),
      Mod = proplists:get_value(module,Args),
      Fun = proplists:get_value(function,Args),
      rpc:call(Target,Mod,Fun,RPCArgs);
    _ ->
      getopt:usage(OptSpec,"rpc.sh")
  end,
  ok.
