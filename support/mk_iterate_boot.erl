#!/usr/bin/env escript
%% -*- mode: erlang -*-
%%! -pa ebin -name nitrogen@localhost
-export([main/1]).

-import(iterate_db).

appname(Tuple) ->
  element(1, Tuple).

appversion(Tuple) ->
  element(3, Tuple).

getappspec(App, List) ->
  {value, AppSpec} = lists:keysearch(App, 1, List),
  AppSpec.

make_rel_tuple(Vsn, ErtsVsn, AppList) ->
  {release, {"iterate", Vsn}, {erts, ErtsVsn},
    [
      getappspec(kernel, AppList),
      getappspec(stdlib, AppList),
      getappspec(sasl, AppList),
      getappspec(mnesia, AppList)
    ]
    
  }.

main(_) ->
    application:start(mnesia),
    application:start(sasl),
    AppList = [{appname(T), appversion(T)} || T <- application:which_applications()],
    RelTuple = make_rel_tuple("0.10.0", "5.6.5", AppList),
    {ok, File} = file:open("iterate.rel", [write]),
    io:fwrite(File, "~p", [RelTuple]),
    io:fwrite(File, ".~n", []).

