-module(story_util).

-include("iterate_records.hrl").

-export([completion/1, complete/1, set_percent/2]).
-export([order/1, set_order/2]).

completion(Story) when is_record(Story, stories) ->
    Meta = Story#stories.meta,
    case lists:keysearch(percent_complete, 1, Meta) of
        {value, {percent_complete, Percent}} ->
            Percent;
        false ->
            0
    end
.

complete(Story) when is_record(Story, stories) ->
    set_percent(Story, 100)
.

set_percent(Story, Num) when is_record(Story, stories) ->
    if
        Num>100 ->
            throw({error, percent_too_high});
        Num<0   ->
            throw({error, percent_too_low});
         true    ->
            Meta = Story#stories.meta
            , Meta2 = lists:keystore(percent_complete, 1, Meta, 
                {percent_complete, Num})
            , Story#stories{meta=Meta2}
    end
.

order(Story) when is_record(Story, stories) ->
    Meta = Story#stories.meta,
    case lists:keysearch(ord, 1, Meta) of
        {value, {ord, Order}} ->
            Order;
        false ->
            0
    end
.

set_order(Story, Num) when is_record(Story, stories) ->
    Meta = Story#stories.meta
    , Meta2 = lists:keystore(ord, 1, Meta, 
        {ord, Num})
    , Story#stories{meta=Meta2}
.

