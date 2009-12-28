-module(iterate_db).

-export([start/0, stop/0, setup/0]).
-export([bootstrap/0, info/0, info/1]).
-export([backlogs/0, stories/1]).
-export([backlog/1, story/1]).
-export([tags/1, tag_delete/2]).
-export([task/1]).
-export([log_time/1]).
-export([iterations/0, iterations/1, iteration/1]).
-export([new_stat/2, new_stat/3]).
-export([stat/1]).
-export([uuid/0, rand/1]).

-include("iterate_records.hrl").
-include("stats.hrl").
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
    , mk_table(stats, record_info(fields, stats))
    , mk_table(tasks, record_info(fields, tasks))
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

%% TODO(jwall): enforce error when new should be update?
backlog({new, Record}) when is_record(Record, backlogs) ->
    backlog({store, Record});
%% TODO(jwall): should this be smarter?
backlog({update, Record}) when is_record(Record, backlogs) ->
    backlog({store, Record});
%% TODO(jwall): need to update all stories for this backlog also
backlog({mutate, Record, RecordMutation}) when is_record(Record, backlogs) ->
    Trans = fun() -> backlog({delete, Record})
        , Stories = story({qry, Record#backlogs.backlog_name})
        , lists:foreach(fun(Story) ->
                story({update, Story#stories{
                    backlog=RecordMutation#backlogs.backlog_name}})
            end, Stories),
        backlog({new, RecordMutation})
    end
    , mnesia:transaction(Trans);
backlog({store, Record}) when is_record(Record, backlogs) ->
    Trans = fun() ->
        mnesia:write(Record)
    end
    , mnesia:transaction(Trans);
backlog({delete, Record}) when is_record(Record, backlogs) ->
    iterate_stats:record(backlog, ?DELETE_STAT(Record#backlogs.backlog_name))
    , Trans = fun() ->
        case Record#backlogs.backlog_name of
            ?DEFAULTB ->
                throw({error, {not_allowed, "Default backlog is permanent"}});
            ?IDEAPOOL ->
                throw({error, {not_allowed, "Idea Pool Backlog is permanent"}});
            Name      ->
                mnesia:delete({backlogs, Name})
        end
        , Stories = story(?Q_STORY(Record#backlogs.backlog_name))
        , lists:foreach(fun(Story) ->
                story({update, Story#stories{
                    backlog=?DEFAULTB}})
            end, Stories)
    end
    , mnesia:transaction(Trans);
backlog(?Q_ALL) ->
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
backlog(?Q_SEARCH_BACKLOG(_Type, "")) ->
    backlog(?Q_ALL);
backlog(?Q_SEARCH_BACKLOG(id, Value)) ->
    backlog({do_search, get_qh(mnesia:table(backlogs)
        , backlog_id_filter(Value))});
backlog(?Q_SEARCH_BACKLOG(desc, Value)) ->
    backlog({do_search, get_qh(mnesia:table(backlogs)
        , backlog_desc_filter(Value))});
backlog(?Q_SEARCH_BACKLOG(all, Value)) ->
    Fid = backlog_id_filter(Value)
    , Fdesc = backlog_desc_filter(Value)
    , F = fun(B) ->
        Fid(B) orelse Fdesc(B)
    end
    , backlog({do_search, get_qh(mnesia:table(backlogs), F)});
backlog({do_search, QH}) ->
    run_qh(QH)
.

backlog_id_filter(Value) ->
    fun
        (B) ->
            Id = case B#backlogs.backlog_name of
                undefined ->
                    "";
                N when is_binary(N) ->
                    binary_to_list(N);
                N ->
                    N
            end
            , string:str(string:to_lower(Id)
                , string:to_lower(Value)) /= 0
    end
.

backlog_desc_filter(Value) ->
    fun
        (B) ->
            Desc = case B#backlogs.desc of
                undefined ->
                    "";
                N when is_binary(N) ->
                    binary_to_list(N);
                N ->
                    N
            end
            , string:str(string:to_lower(Desc)
                , string:to_lower(Value)) /= 0
    end
.

iterations() ->
    iteration(?Q_ALL)
.

iterations(all) ->
    F = fun (_) ->
        true
    end
    , run_iteration_qh(F);
iterations(started) ->
    F = fun (I) ->
        iteration_util:started(I)
    end
    , run_iteration_qh(F);
iterations(closed) ->
    F = fun (I) ->
        not iteration_util:started(I)
    end
    , run_iteration_qh(F)
.

%TODO(jwall) do the same for all the queries
run_iteration_qh(F) when is_function(F, 1) ->
    QH = get_qh(mnesia:table(iterations), F)
    , run_qh(QH)
.

iteration(?NEWITER(Name, Desc)) ->
    Iter = iteration_util:start(#iterations{iteration_name=Name, desc=Desc})
    , iteration(?STOREITER(Iter));
iteration(?UPDATEITER(Iter)) ->
    iteration(?STOREITER(Iter));
iteration(?STOREITER(Iter)) when is_record(Iter, iterations) ->
    Trans = fun() ->
        mnesia:write(Iter)
    end
    , mnesia:transaction(Trans);
iteration(?DELITER(Name)) ->
    iterate_stats:record(iteration, ?DELETE_STAT(Name))
    , Trans = fun() ->
        mnesia:delete({iterations, Name})
    end
    , mnesia:transaction(Trans);
iteration(?Q_SEARCH_BACKLOG(_Type, "")) ->
    iteration(?Q_ALL);
iteration(?Q_SEARCH_BACKLOG(id, Value)) ->
    iteration({do_search, get_qh(mnesia:table(iterations)
        , iteration_id_filter(Value))});
iteration(?Q_SEARCH_BACKLOG(desc, Value)) ->
    iteration({do_search, get_qh(mnesia:table(iterations)
        , iteration_desc_filter(Value))});
iteration(?Q_SEARCH_BACKLOG(all, Value)) ->
    Fid = iteration_id_filter(Value)
    , Fdesc = iteration_desc_filter(Value)
    , F = fun(B) ->
        Fid(B) orelse Fdesc(B)
    end
    , iteration({do_search, get_qh(mnesia:table(iterations), F)});
iteration({do_search, QH}) ->
    run_qh(QH);
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

iteration_id_filter(Value) ->
    fun
        (B) ->
            Id = case B#iterations.iteration_name of
                undefined ->
                    "";
                N when is_binary(N) ->
                    binary_to_list(N);
                N ->
                    N
            end
            , string:str(string:to_lower(Id)
                , string:to_lower(Value)) /= 0
    end
.

iteration_desc_filter(Value) ->
    fun
        (B) ->
            Desc = case B#iterations.desc of
                undefined ->
                    "";
                N when is_binary(N) ->
                    binary_to_list(N);
                N ->
                    N
            end
            , string:str(string:to_lower(Desc)
                , string:to_lower(Value)) /= 0
    end
.
story({delete, Record}) when is_record(Record, stories) ->
    iterate_stats:record(story, ?DELETE_STAT(Record#stories.story_name))
    , Trans = fun() ->
        mnesia:delete({stories, Record#stories.story_name})
    end
    , mnesia:transaction(Trans);
story({new, Record}) when is_record(Record, stories) ->
    story({store, Record});
story(?Q_UPDATE_STORY(Record)) when is_record(Record, stories) ->
    story({store, Record});
story({store, Record}) when is_record(Record, stories) ->
    Trans = fun() ->
        case Record#stories.backlog of
            undefined ->
            %TODO(jwall): handle this taking into account iterations?
                mnesia:write(Record);
                %, throw({error, needs_backlog});
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
story(?Q_ALL) ->
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
story(?Q_BACKLOG_STORY(Name)) ->
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
    end;
story(?Q_ITERATION_STORY(Name)) ->
    Trans = fun() ->
        case iteration(?Q_ITERATION(Name)) of
            [] ->
                [];
            [_Iter] ->
                QH = qlc:q([S || S <-
                    mnesia:table(stories)
                    , story_util:iteration(S) == Name])
                , qlc:eval(QH);
            Msg ->
                throw({error, Msg})
        end
    end
    , case mnesia:transaction(Trans) of
        {atomic, RecordList} ->
            story_util:sort(ord, RecordList);
        {abort, Msg} ->
            {error, Msg};
        Msg ->
            throw({error, Msg})
    end
.

stories(B) ->
    story(?Q_BACKLOG_STORY(B))
.

task(?Q_STORY_TASKS(For)) ->
    Trans = fun() ->
        mnesia:match_object(#tasks{story_name=For, _='_'})
    end
    , mnesia:transaction(Trans);
task(?Q_TASK(Id)) ->
    Trans = fun() ->
        mnesia:match_object(#tasks{id=Id, _='_'})
    end
    , mnesia:transaction(Trans);
task(?C_NEW_TASK(For, Name)) ->
    Trans = fun() ->
        Id = uuid()
        , mnesia:write(#tasks{id=Id, task_name=Name, story_name=For})
        , Id
    end
    , mnesia:transaction(Trans);
task(?U_TASK(Record)) when is_record(Record, tasks) ->
    Trans = fun() ->
        mnesia:write(Record)
    end
    , mnesia:transaction(Trans);
task(?D_TASK(Id)) ->
    Trans = fun() ->
        mnesia:delete({tasks, Id})
    end
    , mnesia:transaction(Trans)
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

tag_delete(Type, For) ->
    Trans = fun() ->
        mnesia:match_object(?TAG(Type, For,'_'))
     end
    , {atomic, Tags} = mnesia:transaction(Trans)
    , DelTrans = fun() ->
        [ mnesia:delete_object(T) || T <- Tags]
    end
    , mnesia:transaction(DelTrans)
.

log_time(?Q_STORY_TIME(Story)) ->
    Trans = fun() -> mnesia:read({time_log, Story}) end
    , case mnesia:transaction(Trans) of
        {atomic, []} ->
            #time_log{story=Story};
        {atomic, [Log]} ->
            Log;
        {abort, Msg} ->
            {error, Msg};
        E ->
            throw({error, E})
    end;
log_time(?UPDATETIME(Story, Amount)) ->
    TS = erlang:universaltime()
    , Trans = fun() ->
        Log = log_time({qry, Story})
        , TLog = Log#time_log.t_series
        %% TODO(jwall): wf:user() crashes when called outside
        %% of the nitrogen web environment
        , mnesia:write(Log#time_log{t_series=[{Amount, TS, wf:user()} | TLog]})
    end
    , mnesia:transaction(Trans);
log_time(?Q_AMT(Story)) ->
    TimeLog = iterate_db:log_time(?Q_STORY_TIME(Story))
    , lists:foldl(fun({T, _TS, _User}, T2) -> T + T2 end, 0.0
        , TimeLog#time_log.t_series)
.

stat(?Q_ALL) ->
    stat(?Q_MATCH_STAT(#stats{_='_'}));
stat({for, For}) ->
    F = fun(S) ->
        case S#stats.type == iteration of
            true ->
                Name = element(2, S#stats.entry)
                , Name2 = element(1, S#stats.entry)
                , Name == For orelse Name2 == For;
            false ->
                false
        end
    end
    , stat(?Q_FILTER_STATS(F));
stat(?Q_MATCH_STAT(MatchSpec)) ->
    Trans = fun() -> mnesia:match_object(MatchSpec) end
    , case mnesia:transaction(Trans) of
        {atomic, RecordList} ->
            RecordList;
        {abort, Msg} ->
            {error, Msg};
        Error ->
            throw({error, Error})
    end;
stat(?Q_FILTER_STATS(F)) ->
    run_qh(get_qh(mnesia:table(stats), F))
.

new_stat(Type, Entry) ->
    new_stat(Type, Entry, wf:user())
.

new_stat(Type, Entry, User) ->
    TS = erlang:now()
    , Trans = fun() ->
        mnesia:write(#stats{id=uuid(), ts=TS, type=Type, user=User
            , entry=Entry})
    end
    , mnesia:transaction(Trans)
.

uuid() ->
   %wf_utils:guid()
   string:join([rand(8), rand(4), rand(4), rand(4), rand(16)], "-")
.

rand(Size) ->
    {S1, S2, S3} = now()
    , random:seed(S1, S2, S3)
    , lists:flatten(
        [
            hd(integer_to_list(random:uniform(10) - 1)) 
                || _N <-  lists:seq(1, Size)])
.

get_qh(Table, F) ->
    qlc:q([I || I <- Table, F(I)])
.

run_qh(QH) ->
    Trans = fun() ->
        qlc:eval(QH)
    end
    , case mnesia:transaction(Trans) of
        {atomic, RecordList} ->
            RecordList;
        {abort, Msg} ->
            {error, Msg}
    end
.

