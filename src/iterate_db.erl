-module(iterate_db).

-export([start/0, stop/0, setup/0]).
-export([bootstrap/0, info/0, info/1]).
-export([backlogs/0, stories/1]).
-export([backlog/1, story/1]).
-export([tags/1]).
-export([log_time/1]).
-export([iterations/0, iteration/1]).

-include("iterate_records.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(DEFAULTB, #backlogs{backlog_name="Default", desc="Default backlog"} ).
-define(IDEAPOOL, #backlogs{backlog_name="Idea Pool", desc="Keep track of ideas and brainstorm"} ).

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
    , mk_table(time_log, record_info(fields, time_log))
    , mk_table(tags, record_info(fields, tags))
    , mk_table(iterations, record_info(fields, iterations))
.

bootstrap() ->
   backlog({new, ?DEFAULTB})
  , backlog({new, ?IDEAPOOL})
  , F = fun(B) ->
       fun(N) -> 
           story({new, #stories{backlog=B, story_name=N, sp=3}})
       end
  end
  , lists:foreach(F("Default"), ["00 - Historical event logging"
                                , "03 - backlog types (iteration/staging)"
                                , "story order"
                                , "story time tracking"
                                ]
  )
  , lists:foreach(F("Idea Pool"), ["01 - Reporting"
                                  , "02 - color code stories"
                                  , "04 - comet updates of the stories and backlogs"
                                  , "06 - tasks for stories"
                                  ]
  )
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

%% TODO(jwall): enforce error when new should be update?
backlog({new, Record}) when is_record(Record, backlogs) ->
    backlog({store, Record});
%% TODO(jwall): should this be smarter?
backlog({update, Record}) when is_record(Record, backlogs) ->
    backlog({store, Record});
%% TODO(jwall): need to update all stories for this backlog also
backlog({mutate, Record, RecordMutation}) when is_record(Record, backlogs) ->
    backlog({delete, Record}),
    Stories = story({qry, Record#backlogs.backlog_name}),
    lists:foreach(fun(Story) -> 
        story({update, Story#stories{backlog=Record#backlogs.backlog_name}})
    end, Stories),
    backlog({new, RecordMutation});
backlog({store, Record}) when is_record(Record, backlogs) ->
    Trans = fun() ->
        mnesia:write(Record)
    end
    , mnesia:transaction(Trans);
backlog({delete, Record}) when is_record(Record, backlogs) ->
    Trans = fun() ->
        %% TODO(jwall): need to move all associated stories to default
        %% TODO(jwall): need to enforce non-delete of Default and Idea Pool
        case Record#backlogs.backlog_name of
            ?DEFAULTB ->
                throw({error, {not_allowed, "Default backlog is permanent"}});
            ?IDEAPOOL ->
                throw({error, {not_allowed, "Idea Pool Backlog is permanent"}});
            Name      ->
                mnesia:delete({backlogs, Name})
        end
    end
    , mnesia:transaction(Trans);
backlog({qry, all}) ->
    Trans = fun() -> mnesia:match_object(#backlogs{_='_'}) end,
    case mnesia:transaction(Trans) of
        {atomic, RecordList} ->
            RecordList;
        {abort, Msg} ->
            {error, Msg};
        Error ->
            throw({error, Error})
    end;
backlog(?Q_BACKLOG(Name)) ->
    Trans = fun() -> mnesia:read({backlogs, Name}) end,
    case mnesia:transaction(Trans) of
        {atomic, RecordList} ->
            RecordList;
        {abort, Msg} ->
            {error, Msg}
    end;
backlog({search, {id, Value}}) ->
    Trans = fun() -> 
        QH = qlc:q([B || B <- 
            mnesia:table(backlogs), string:str(string:to_lower(?BNAME(B)), 
                string:to_lower(Value)) /= 0]),
        qlc:eval(QH)
    end,
    case mnesia:transaction(Trans) of
        {atomic, RecordList} ->
            RecordList;
        {abort, Msg} ->
            {error, Msg}
    end
.

iterations() ->
    iteration(?Q_ALL)
.

iteration(?Q_ALL) ->
    Trans = fun() ->
        mnesia:match_object(#iterations{_='_'})
    end
    , case mnesia:transaction(Trans) of
        {atomic, RecordList} ->
            RecordList;
        {abort, Msg} ->
            {error, Msg};
        Error ->
            throw({error, Error})
    end;
iteration(?NEWITER(Name, Desc)) ->
    iteration(?STOREITER(#iterations{iteration_name=Name, desc=Desc}));
iteration(?UPDATEITER(Iter)) ->
    iteration(?STOREITER(Iter));
iteration(?STOREITER(Iter)) when is_record(Iter, iterations) ->
    Trans = fun() ->
        mnesia:write(Iter)
    end
    , mnesia:transaction(Trans);
iteration(?DELITER(Name)) ->
    Trans = fun() ->
        mnesia:delete({iterations, Name})
    end
    , mnesia:transaction(Trans);
iteration(?Q_ITERATION(Name)) ->
    Trans = fun() ->
        mnesia:read({iterations, Name})
    end
    , case mnesia:transaction(Trans) of
        {atomic, RecordList} ->
            RecordList;
        {abort, Msg} ->
            {error, Msg};
        Msg ->
            throw({error, Msg})
    end
.

story({delete, Record}) when is_record(Record, stories) ->
    Trans = fun() ->
        mnesia:delete({stories, Record#stories.story_name})
    end
    , mnesia:transaction(Trans);
story({new, Record}) when is_record(Record, stories) ->
    story({store, Record});
story({update, Record}) when is_record(Record, stories) ->
    story({store, Record});
story({store, Record}) when is_record(Record, stories) ->
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
                        %% handle the already exists case?
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
story(?Q_STORY(Name)) ->
    Trans = fun() -> mnesia:read({stories, Name}) end,
    case mnesia:transaction(Trans) of
        {atomic, RecordList} ->
            RecordList;
        {abort, Msg} ->
            {error, Msg}
    end;
story({qry, {backlog, Name}}) ->
    Trans = fun() -> mnesia:match_object(#stories{backlog=Name, _='_'}) end,
    case mnesia:transaction(Trans) of
        {atomic, []} ->
            [];
        {atomic, RecordList} ->
            story_util:sort(ord, RecordList);
        {abort, Msg} ->
            {error, Msg};
        E ->
            throw({error, E})
    end
.

stories(B) ->
    story({qry, {backlog, B}})
.

tags(?NEWTAG(story, For, Value)) ->
    Trans = fun() ->
        %% look for story before adding tag
        case story(?Q_STORY(For)) of
            [_Story] ->
                mnesia:write(?STAG(For, Value));
            [] ->
                throw({error, no_such_story})
        end
    end
    , mnesia:transaction(Trans);
tags(?NEWTAG(backlog, For, Value)) ->
    Trans = fun() ->
        %% look for backlog before adding tag
        case backlog(?Q_BACKLOG(For)) of
            [_Backlog] ->
                mnesia:write(?BTAG(For, Value));
            [] ->
                throw({error, no_such_backlog})
        end
    end
    , mnesia:transaction(Trans);
tags(?Q_TAGS(Type, For)) ->
    Trans = fun() ->
        mnesia:match_object(?TAG(Type, For,'_'))
     end
    , mnesia:transaction(Trans)
.

log_time({qry, Story}) ->
    Trans = fun() -> mnesia:read({time_log, Story}) end
    , case mnesia:transaction(Trans) of
        {atomic, [Log]} ->
            Log;
        {atomic, []} ->
            #time_log{story=Story};
        {abort, Msg} ->
            {error, Msg};
        E ->
            throw({error, E})
    end;
log_time({Story, Amount}) ->
    TS = erlang:universaltime()
    , Trans = fun() ->
        Log = log_time({qry, Story})
        , TLog = Log#time_log.t_series
        , mnesia:write(Log#time_log{t_series=[{Amount, TS} | TLog]})
    end
    , mnesia:transaction(Trans)
.

