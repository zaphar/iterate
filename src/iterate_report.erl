-module(iterate_report).
-compile(export_all).

-include("stats.hrl").
-include("iterate_records.hrl").

-export([completion/1, completion_for_last_week/1]).
-export([story_points_completed/1, story_points_completed_for_last_week/1]).
-import(date_util, [now_to_milliseconds_hires/1, is_time_older_than/2
    , is_older_by/3]).

completion_for_last_week({Type, Name}) ->
    stats_for_last_week(percent, Type, Name)
.

completion({Type, Name}) ->
    stats(percent, Type, Name)
.

story_points_completed_for_last_week({Type, Name}) ->
    stats_for_last_week(complete_sp, Type, Name)
.

story_points_completed({Type, Name}) ->
    stats(complete_sp, Type, Name)
.

stats(Kind, Type, Name) ->
    F = mk_is_change_stat_fun(Name, Kind, Type)
    , Sort = make_sorter_desc()
    , get_stats(F, Sort, mk_translater(Kind))
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
    , get_stats(F, Sort,  mk_translater(Kind))
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
        (Stat) when is_record(Stat, stats), Stat#stats.for == Type ->
             F(Stat#stats.entry);
        (_) ->
            false
    end
.

get_date_time_for_stat(S) ->
        calendar:now_to_local_time(S#stats.ts)
.

