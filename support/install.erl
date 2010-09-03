#!/usr/bin/env escript
%% -*- mode: erlang -*-
%%! -pa ../ebin
-export([main/1]).

-import(target_system).

main([]) ->
  with_args();
main([Arg1]) ->
  with_args(Arg1);
main([Arg1, Arg2]) ->
  with_args(Arg1, Arg2).

with_args() ->
  with_args("iterate").

with_args("rel="++Rel) ->
  with_args(Rel);
with_args("root="++Root) ->
  with_args("iterate", Root);
with_args(Rel) ->
  with_args(Rel, "/usr/local/iterate").

with_args("rel="++Rel, "root="++Root) ->
  with_args(Rel, Root);
with_args("root="++Root, "rel="++Rel) ->
  with_args(Rel, Root);
with_args(Rel, Root) ->
  do_install({rel, Rel}, {root, Root}).

do_install({rel, Rel}, {root, Root}) ->
  target_system:install(Rel, Root).
