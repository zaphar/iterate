-module(iterate_db_test).
-compile(export_all).

-import(etap_can, [loaded_ok/2, can_ok/2, can_ok/3]).

-include("../src/iterate_records.hrl").

start() ->
    etap:plan(22)
    , module_test()
    , backlogs_test()
    , stories_test()
    , start_up_shutdown_test()
    , info_test()
    , create_table_test()
    , mk_backlog_test()
.

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
.

backlogs_test() ->
    etap:is(iterate_db:backlogs(), ["Default", "Mine"], "got the backlogs").

stories_test() ->
    etap:is(iterate_db:stories("Default"), ["Story One"], "got the story")
    , etap:is(iterate_db:stories("foo"), [], "got no stories").

start_up_shutdown_test() ->
    iterate_db:start()
    , etap:is(mnesia:system_info(is_running), yes, "the database is running")
    , iterate_db:stop()
    , etap:is(mnesia:system_info(is_running), no, "the database is not running")
.

info_test() ->
    mnesia:stop()
    , etap:is(iterate_db:info(is_running), no
        , "we can ask the database if it's running or not")
.

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

mk_backlog_test() ->
    Result = iterate_db:backlog({new, 
        #backlogs{backlog_name="Default", desc="a desc not a desk"}})
    , etap:is({atomic, ok}, Result, "yep we made the record")
    , [Record | RecordList] = iterate_db:backlog({qry, "Default"})
    , etap:ok(is_record(Record, backlogs), "we got back a backlogs record")
    , etap:is(Record#backlogs.desc, "a desc not a desk"
        , "the record has our description")
    , [Record1 | _T] = iterate_db:backlog({qry, all})
    , etap:is(Record, Record1, "backlog qry for all has Record in it")
.

