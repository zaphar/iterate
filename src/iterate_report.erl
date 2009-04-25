-module(iterate_report).

-include("stats.hrl").
-include("iterate_records.hrl").

-export([completion/1]).

-define(TIMESERIES(Date, Time, Value), {Date, Time, Value}).

completion({iteration, Iter}) ->
    IsForIter = fun
        (?CHANGE_STAT(IterName, percent, _)) ->
            Iter == IterName;
        (_) ->
            false
    end
    , F = fun
        (Stat) when is_record(Stat, stats), Stat#stats.for == iteration ->
             IsForIter(Stat#stats.entry);
        (_) ->
            false
    end
    , [ translate_completion_stats(S) ||
        S <- iterate_db:stat(?Q_FILTER_STATS(F))]
.

translate_completion_stats(S) when is_record(S, stats) ->
    {Date, Time} = calendar:now_to_local_time(S#stats.ts)
    , ?CHANGE_STAT(_For, percent, Value) = S#stats.entry
    , ?TIMESERIES(Date, Time, {percent, Value})
.

