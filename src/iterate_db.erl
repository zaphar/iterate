-module(iterate_db).

-export([start/0, stop/0, setup/0, bootstrap/0, info/0, info/1]).
-export([backlogs/0, stories/1]).
-export([backlog/1, story/1]).

-include("iterate_records.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(DEFAULTB, #backlogs{backlog_name="Default", desc="Default backlog"} ).
-define(IDEAPOOL, #backlogs{backlog_name="Idea_Pool", desc="Keep track of ideas and brainstorm"} ).

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
    , bootstrap()
.

bootstrap() ->
   backlog({new, ?DEFAULTB})
  , backlog({new, ?IDEAPOOL})
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
    backlog({qry, all})
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

story({new, Record}) when is_record(Record, stories) ->
    Trans = fun() ->
        case Record#stories.backlog of
            undefined ->
                throw({error, needs_backlog});
            Name ->
                case backlog({qry, Name}) of
                    [] ->
                        throw({error, no_such_backlog});
                    {error, Msg} ->
                        throw({error, Msg});
                     _ ->
                        mnesia:write(Record)
                end
        end
    end
    , mnesia:transaction(Trans);
story({qry, all}) ->
    Trans = fun() -> mnesia:match_object(#stories{_='_'}) end,
    case mnesia:transaction(Trans) of
        {atomic, RecordList} ->
            RecordList;
        {abort, Msg} ->
            {error, Msg};
        _ ->
            {error, "whoah what was that?"}
    end;
story({qry, B}) ->
    Trans = fun() -> mnesia:match_object(#stories{backlog=B, _='_'}) end,
    case mnesia:transaction(Trans) of
        {atomic, RecordList} ->
            RecordList;
        {abort, Msg} ->
            {error, Msg};
        _ ->
            throw({error, "whoah what was that?"})
    end
.

stories("Default") ->
    ["add and delete stories"
     , "add and delete backlogs"
     , "storage layer for backlogs and data"
     , "drag drop stories to backlogs"
     , "backlog filter and search"];
stories("Idea_Pool") ->
    ["Story One"];
stories(B) when is_list(B) ->
    []
.
