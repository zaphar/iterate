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
    , lists:sort(fun({A1, A2, _}, {B1, B2, _}) ->
            is_time_older_than({A1, A2}, {B1, B2})
        end
        , [ translate_completion_stats(S) ||
        S <- iterate_db:stat(?Q_FILTER_STATS(F))])
.

translate_completion_stats(S) when is_record(S, stats) ->
    {Date, Time} = calendar:now_to_local_time(S#stats.ts)
    , ?CHANGE_STAT(_For, percent, Value) = S#stats.entry
    , ?TIMESERIES(Date, Time, {percent, Value})
.

is_time_older_than({Date, Time}, Mark) ->
    is_time_older_than(calendar:datetime_to_gregorian_seconds({Date, Time})
        , Mark);
is_time_older_than(Time, {DateMark, TimeMark}) ->
    is_time_older_than(Time
        , calendar:datetime_to_gregorian_seconds({DateMark, TimeMark}));
is_time_older_than(Time, Mark)  when is_integer(Time), is_integer(Mark) ->
    Time < Mark
.

%is_time_sooner_than({Date, Time}, Mark) ->
%    is_time_sooner_than(calendar:datetime_to_gregorian_seconds({Date, Time})
%        , Mark);
%is_time_sooner_than(Time, {DateMark, TimeMark}) ->
%    is_time_sooner_than(Time
%        , calendar:datetime_to_gregorian_seconds({DateMark, TimeMark}));
%is_time_sooner_than(Time, Mark)  when is_integer(Time), is_integer(Mark) ->
%    Time > Mark
%.

