#!/usr/bin/env escript
%% -*- mode: erlang -*-
%%! -pa ebin
-export([main/1]).

-import(iterate_db).

main(_) ->
    iterate_db:setup().
