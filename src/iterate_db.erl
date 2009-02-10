-module(iterate_db).

-export([start/0, stop/0, setup/0, info/0, info/1]).
-export([backlogs/0, stories/1]).
-export([backlog/1]).

-include("../iterate_records.hrl").
-include_lib("stdlib/include/qlc.hrl").

start() ->
    mnesia:start()
.

stop() ->
    mnesia:stop()
.

setup() ->
    stop()
    , mnesia:create_schema([node()])
    , start()
    , mk_table(backlogs, record_info(fields, backlogs))
    , mk_table(stories, record_info(fields, stories))
.

mk_table(Name, Info) ->
    mnesia:create_table(Name, [
        {type, ordered_set}
        , {disc_copies, [node()]}
        , {attributes, Info}
    ])
.

info() ->
    mnesia:system_info()
.

info(Ask) when is_atom(Ask) ->
    mnesia:system_info(Ask)
.

backlogs() ->
    ["Default", "Mine"]
.

backlog({new, Record}) when is_record(Record, backlogs) ->
    Trans = fun() ->
        mnesia:write(Record)
    end
    , mnesia:transaction(Trans);
backlog({qry, all}) ->
    Trans = fun() -> mnesia:match_object(#backlogs{_='_'}) end,
    case mnesia:transaction(Trans) of
        {atomic, RecordList} ->
            RecordList;
        {abort, Msg} ->
            {error, Msg};
        _ ->
            {error, "whoah what was that?"}
    end;
backlog({qry, Name}) ->
    Trans = fun() -> mnesia:read({backlogs, Name}) end,
    case mnesia:transaction(Trans) of
        {atomic, RecordList} ->
            RecordList;
        {abort, Msg} ->
            {error, Msg}
    end
.

stories("Default") ->
    ["Story One"];
stories("Mine") ->
     ["add and delete stories"
      , "add and delete backlogs"
      , "storage layer for backlogs and data"
      , "drag drop stories to backlogs"
      , "backlog filter and search"];
stories(B) when is_list(B) ->
    []
.
