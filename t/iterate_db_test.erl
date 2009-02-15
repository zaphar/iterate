-module(iterate_db_test).
-compile(export_all).

-import(etap_can, [loaded_ok/2, can_ok/2, can_ok/3]).

-include("../src/iterate_records.hrl").
-include_lib("etap/include/etap.hrl").
-define(DEFAULTB, #backlogs{backlog_name="Default", desc="Default backlog"} ).
-define(MINEB, #backlogs{backlog_name="Mine", desc="Development Backlog"} ).

start() ->
    calc_plan
    , module_test()
    , start_up_shutdown_test()
    , info_test()
    , create_table_test()
    , mk_backlog_test()
    , backlogs_test()
    , stories_test()
    , make_story_test()
.

-plan(8).
module_test() ->
    loaded_ok(iterate_db, "iterate_db module loaded ok")
    , can_ok(iterate_db, backlogs, 0)
    , can_ok(iterate_db, stories, 1)
    , can_ok(iterate_db, info, 0)
    , can_ok(iterate_db, info, 1)
    , can_ok(iterate_db, start, 0)
    , can_ok(iterate_db, stop, 0)
    , can_ok(iterate_db, setup, 0)
    , can_ok(iterate_db, backlog, 1)
    , can_ok(iterate_db, story, 1)
.

-plan(2).
start_up_shutdown_test() ->
    iterate_db:start()
    , etap:is(mnesia:system_info(is_running), yes, "the database is running")
    , iterate_db:stop()
    , etap:is(mnesia:system_info(is_running), no, "the database is not running")
.

-plan(1).
info_test() ->
    mnesia:stop()
    , etap:is(iterate_db:info(is_running), no
        , "we can ask the database if it's running or not")
.

-plan(3).
create_table_test() ->
    iterate_db:setup()
    , etap:is(iterate_db:info(is_running), yes, "the database is running")
    , create_table_test(backlogs)
    , create_table_test(stories)
.

create_table_test(Table) ->
    F = fun(Any) ->
        case Any of
            Table ->
                true;
            _ ->
                false
        end
    end
    , etap:any(F, iterate_db:info(tables)
        , io_lib:format("we have a ~s table", [Table]))
.

-plan(4).
mk_backlog_test() ->
    Result = iterate_db:backlog({new, ?DEFAULTB})
    , etap:is({atomic, ok}, Result, "yep we made the record")
    , [Record | RecordList] = iterate_db:backlog({qry, "Default"})
    , etap:ok(is_record(Record, backlogs), "we got back a backlogs record")
    , etap:is(Record, ?DEFAULTB, "the record has our description")
    , [Record1 | _T] = iterate_db:backlog({qry, all})
    , etap:is(Record, Record1, "backlog qry for all has Record in it")
    , iterate_db:backlog({new, ?MINEB})
.

-plan(2).
backlogs_test() ->
    F = fun(Backlog) ->
        fun(Any) ->
            case Any of
                Backlog ->
                    true;
                _ ->
                    false
            end
        end
    end
    , etap:any(F(?DEFAULTB), iterate_db:backlogs(), "got ?DEFAULTB in the backlogs")
    , etap:any(F(?MINEB), iterate_db:backlogs(), "got ?MINEB in the backlogs")
.

make_story_test() ->
    Result = iterate_db:story({new, 
        #stories{backlog="Default", story_name=foo, sp=3
                 , desc="bar"}})
    , etap:is(Result, {atomic, ok}, "the story got added")
    , etap:diag(io_lib:format("~p", [Result]))
    , F = fun() -> iterate_db:story({new,
        #stories{story_name=foo, sp=3, desc="bar"}})
    end
    , etap:diag(io_lib:format("~p", [F()]))
    , etap:is(F(),  {aborted, {throw, {error, needs_backlog}}}, 
        "story with no backlog fails")
    , F1 = fun() -> iterate_db:story({new,
        #stories{backlog=bar, story_name=foo, sp=3, desc="bar"}})
    end
    , etap:diag(io_lib:format("~p", [F1()]))
    , etap:is(F1(),  {aborted, {throw, {error, no_such_backlog}}}, 
        "story with non existent backlog fails")
.

-plan(2).
stories_test() ->
    etap:skip(fun() -> 
            etap:is(iterate_db:stories("Default"), ["Story One"], "got the story")
            , etap:is(iterate_db:stories("foo"), [], "got no stories")
        end, "not implemented yet")
    .

