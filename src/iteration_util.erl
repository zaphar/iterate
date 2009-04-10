-module(iteration_util).
-compile(export_all).

-include("iterate_records.hrl").

start(Iter) when is_record(Iter, iterations) ->
    Meta = Iter#iterations.meta
    , Meta2 = lists:keystore(started, 1, Meta, {started, true})
    , Iter#iterations{meta=Meta2}
.

started(Iter) when is_record(Iter, iterations) ->
    Meta = Iter#iterations.meta
    , case lists:keysearch(started, 1, Meta) of
        {value, {started, true}} ->
            true;
        _ ->
            false
    end
.

close(Iter) when is_record(Iter, iterations) ->
    Meta = Iter#iterations.meta
    %% TODO(jwall): need to move any incomplete stories out into
    %%              Default backlog;
    , Meta2 = lists:keystore(started, 1, Meta, {started, false})
    , Iter#iterations{meta=Meta2}
.
