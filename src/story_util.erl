-module(story_util).

-include("iterate_records.hrl").

-export([completion/1, complete/1, set_percent/2]).
-export([order/1, set_order/2]).
-export([sort/2]).
-export([iteration/1, set_iteration/2]).
-export([set_backlog/2, get_type/1]).

completion(Story) when is_record(Story, stories) ->
    get_meta(Story#stories.meta, percent_complete)
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
    get_meta(Story#stories.meta, ord)
.

get_type(Story) when is_record(Story, stories) ->
    case iteration(Story) of
        0 ->
            {backlog, backlog(Story)};
        Name ->
            {iteration, Name}
    end
.

iteration(Story) when is_record(Story, stories) ->
    get_meta(Story#stories.meta, iteration)
.

set_iteration(Story, Name) when is_record(Story, stories) ->
    Story#stories{meta=update_meta(Story#stories.meta, iteration, Name)
        , backlog=undefined}
.

set_backlog(Story, Name) when is_record(Story, stories) ->
   NewStory = set_iteration(Story, 0)
   , NewStory#stories{backlog=Name} 
.

backlog(Story) when is_record(Story, stories) ->
    Story#stories.backlog
.

set_order(Story, Num) when is_list(Num) ->
    set_order(Story, list_to_integer(Num));
set_order(Story, Num) when is_record(Story, stories) ->
    Story#stories{meta=update_meta(Story#stories.meta, ord, Num)}
.

sort(ord, StoryList) ->
    sort({ord, asc}, StoryList);
sort({ord, desc}, StoryList) ->
    lists:sort(fun(S1, S2) ->
            order(S1) > order(S2)
        end, StoryList);
sort({ord, asc}, StoryList) ->
    lists:sort(fun(S1, S2) ->
            order(S2) > order(S1)
        end, StoryList)
.

get_meta(Meta, Key) ->
    case lists:keysearch(Key, 1, Meta) of
        {value, {Key, Value}} ->
            Value;
        false ->
            0
    end
.

update_meta(Meta, Key, Value) ->
    lists:keystore(Key, 1, Meta, 
        {Key, Value})
.

