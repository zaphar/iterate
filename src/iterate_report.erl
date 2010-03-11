-module(iterate_report).
-compile(export_all).

-include("stats.hrl").
-include("iterate_records.hrl").

-import(date_util, [now_to_milliseconds_hires/1, is_time_older_than/2
    , is_older_by/3]).

completion_for_last_week({Type, Name}) ->
    stats_for_last_week(percent, Type, Name)
.

completion({Type, Name}) ->
    stats(percent, Type, Name)
.

story_points_incomplete_for_last_week({Type, Name}) ->
    stats_for_last_week(incomplete_sp, Type, Name)
.

story_points_completed_for_last_week({Type, Name}) ->
    stats_for_last_week(complete_sp, Type, Name)
.

story_points_completed({Type, Name}) ->
    stats(complete_sp, Type, Name)
.

tag_spread({backlog, Name}) ->
    TagList = iterate_wf:backlog_tags(Name)
    , tag_spread({taglist, TagList});
tag_spread({iteration, Name}) ->
    TagList = iterate_wf:iteration_tags(Name)
    , tag_spread({taglist, TagList});
tag_spread({taglist, TagList}) ->
    lists:foldl(
        fun
            (T, {Total, []}) ->
               {Total + 1, [{T, 1}]}; 
            (T, {Total, L}) ->
                Count = case lists:keysearch(T, 1, L) of
                    false ->
                        0;
                    {value, {T, N}} ->
                       N 
                end
                , {Total + 1, lists:keystore(T, 1, L, {T, Count+1})}
        end
        , {0, []}, TagList);
tag_spread({_, _}) ->
    {0, []}
.

stats(Kind, Type, Name) ->
    F = mk_is_change_stat_fun(Name, Kind, Type)
    , Sort = make_sorter_desc()
    , case get_stats(F, Sort,  mk_translater(Kind)) of
        [] ->
            [current_stat(Type, Name, Kind)];
        L  ->
            L ++ [current_stat(Type, Name, Kind)] 
    end
.

stats_for_last_week(Kind, Type, Name) ->
    {Now, _} = calendar:local_time()
    , F1 = mk_is_change_stat_fun(Name, Kind, Type)
    , F2 = fun(S) ->
        {Date, _} = get_date_time_for_stat(S)
        , is_older_by(Date, Now, {days, 7}) orelse Date == Now
    end
    , F = fun(S) ->
        F1(S) and F2(S)
    end
    , Sort = make_sorter_desc()
    %% TODO(jwall): refactor this into get_stats instead?
    , case get_stats(F, Sort,  mk_translater(Kind)) of
        [] ->
            [current_stat(Type, Name, Kind)];
        L  ->
            L ++ [current_stat(Type, Name, Kind)] 
    end
.

current_stat(Type, Name, Kind) ->
    Date = date()
    , Time = time()
    , Epoch = date_util:now_to_milliseconds()
    , current_stat({Type, Name}, Kind, Date, Time, Epoch)
.

current_stat({iteration, Name}, incomplete_sp, Date, Time, Epoch) ->
    {_, Incomplete} = iterate_wf:iteration_story_points(Name)
    , ?TIMESERIES(trunc(Epoch), Date, Time
            , Incomplete, incomplete_sp);
current_stat({iteration, Name}, complete_sp, Date, Time, Epoch) ->
    {Complete, _} = iterate_wf:iteration_story_points(Name)
    , ?TIMESERIES(trunc(Epoch), Date, Time
            , Complete, complete_sp);
current_stat({backlog, Name}, incomplete_sp, Date, Time, Epoch) ->
    {_, Incomplete} = iterate_wf:backlog_story_points(Name)
    , ?TIMESERIES(trunc(Epoch), Date, Time
            , Incomplete, incomplete_sp);
current_stat({backlog, Name}, complete_sp, Date, Time, Epoch) ->
    {Complete, _} = iterate_wf:backlog_story_points(Name)
    , ?TIMESERIES(trunc(Epoch), Date, Time
            , Complete, complete_sp);
current_stat({backlog, Name}, percent, Date, Time, Epoch) ->
    Value = iterate_wf:backlog_completion(Name)
    , ?TIMESERIES(trunc(Epoch), Date, Time
            , Value, percent);
current_stat({iteration, Name}, percent, Date, Time, Epoch) ->
    Value = iterate_wf:iteration_completion(Name)
    , ?TIMESERIES(trunc(Epoch), Date, Time
            , Value, percent);
current_stat({story, Name}, complete_sp, Date, Time, Epoch) ->
    Story = iterate_wf:get_story(Name)
    , ?TIMESERIES(trunc(Epoch), Date, Time
            , Story#stories.sp, percent);
current_stat({story, Name}, percent, Date, Time, Epoch) ->
    [Story] = iterate_wf:get_story(Name)
    , Value = story_util:completion(Story)
    , ?TIMESERIES(trunc(Epoch), Date, Time
            , Value, percent)
.

get_stats(F, Sort, Transform) ->
    lists:sort(Sort, [ Transform(S) ||
        S <- iterate_db:stat(?Q_FILTER_STATS(F))])
.

%% stat utils

make_sorter_desc() ->
    fun(TS1, TS2) ->
        is_time_older_than({TS1#tsentry.date, TS1#tsentry.time}
            , {TS2#tsentry.date, TS2#tsentry.time})
    end
.

mk_translater(Kind) ->
    fun(S) when is_record(S, stats) ->
        {Date, Time} = calendar:now_to_local_time(S#stats.ts)
        , Epoch = now_to_milliseconds_hires(S#stats.ts)
        , ?CHANGE_STAT(_For, Kind, Value) = S#stats.entry
        , ?TIMESERIES(trunc(Epoch), Date, Time
            , Value, Kind)
    end
.

mk_completion_translater() -> mk_translater(percent).

mk_is_change_stat_fun(Name, StatType, Type) ->
    F = fun
        (?CHANGE_STAT(ForName, ThisStatType, _)) 
            when StatType == ThisStatType ->
                Name == ForName;
        (_) ->
            false
    end
    , mk_is_stat_type_fun(Type, F)
.

mk_is_stat_type_fun(Type, F) ->
    fun
        (Stat) when is_record(Stat, stats), Stat#stats.type == Type ->
             F(Stat#stats.entry);
        (_) ->
            false
    end
.

get_date_time_for_stat(S) when is_record(S, stats) ->
        calendar:now_to_local_time(S#stats.ts)
.

