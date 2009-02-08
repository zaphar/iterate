-module(iterate_db_test).

-export([start/0]).
-import(etap_can, [loaded_ok/2, can_ok/2, can_ok/3]).

start() ->
    etap:plan(5),
    module_test(),
    backlogs_test(),
    stories_test().

module_test() ->
    loaded_ok(iterate_db, "iterate_db module loaded ok"),
    can_ok(iterate_db, backlogs, 0),
    can_ok(iterate_db, stories, 1).

backlogs_test() ->
    etap:is(iterate_db:backlogs(), ["Default", "Mine"], "got the backlogs").

stories_test() ->
    etap:is(iterate_db:stories("Default"), ["Story One"], "got the story"),
    etap:is(iterate_db:stories("Mine"), [], "got no stories").
