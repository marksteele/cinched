#!/usr/bin/env escript
%% -*- erlang -*-

main(_Args) ->
  Root = filename:dirname(filename:dirname(escript:script_name())),
  ReleasePath = filename:join(Root,"rel/cinched"),
  io:format("Adding code paths~n",[]),
  true = code:add_path(filename:join(Root,"ebin")),
  true = code:add_path(filename:join(Root,"deps/erlware_commons/ebin")),
  io:format("Removing previous build~n",[]),
  ec_file:remove(ReleasePath,[recursive]),
  io:format("Creating empty release~n",[]),
  ok = filelib:ensure_dir(filename:join(ReleasePath,"dummy")),
  io:format("Reading release config~n",[]),
  {ok, Conf} = file:consult(filename:join(Root,"rel/reltool.config")),
  io:format("Getting target specs~n",[]),
  {ok, Spec} = reltool:get_target_spec(Conf),
  io:format("Creating release~n",[]),
  reltool:eval_target_spec(Spec, code:root_dir(), ReleasePath),
  io:format("All done~n",[]).
