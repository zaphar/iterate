-module(iteration_util).
-compile(export_all).

-include("iterate_records.hrl").

stories(Iter) when is_record(Iter, iterations) ->
    Meta = Iter#iterations.meta
    , case lists:keysearch(stories, 1, Meta) of
        {value, {stories, List}} when is_list(List) ->
            List;
        _ ->
            []
    end
.

start(Iter) when is_record(Iter, iterations) ->
    Meta = Iter#iterations.meta
    , Meta2 = lists:keystore(started, 1, Meta, {started, true})
    , Iter#iterations{meta=Meta2}
.

stop(Iter) when is_record(Iter, iterations) ->
    Meta = Iter#iterations.meta
    , Meta2 = lists:keystore(started, 1, Meta, {started, false})
    , Iter#iterations{meta=Meta2}
.
