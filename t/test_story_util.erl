-module(test_story_util).

-include_lib("etap/include/etap.hrl").
-include("../src/iterate_records.hrl").

-export([start/0]).

start() ->
    calc_plan
    , test_completion()
    , test_complete()
    , test_order()
    , test_sort_by_order_asc()
    , test_sort_by_order_desc()
    , etap:end_tests()
.

-plan(4).
test_completion() ->
    Story = #stories{meta=[{percent_complete, 10}]}
    , etap:is(story_util:completion(Story), 10, "the story completion matched")
    , Story2 = story_util:set_percent(Story, 30)
    , etap:is(story_util:completion(Story2), 30, "successfully set percentage")
    , etap_exception:throws_ok(fun() -> story_util:set_percent(Story, 101) end,
        {error, percent_too_high},
        "dies when trying to set percent to high")
    , etap_exception:throws_ok(fun() -> story_util:set_percent(Story, -1) end,
        {error, percent_too_low},
        "dies when trying to set percent to low")
.

-plan(1).
test_complete() ->
    Story = #stories{meta=[{percent_complete, 10}]}
    , Story2 = story_util:complete(Story)
    , etap:is(story_util:completion(Story2), 100, "completed the story")
.

-plan(2).
test_order() ->
    Story = #stories{meta=[{ord, 10}]}
    , etap:is(story_util:order(Story), 10, "the story order matched")
    , Story2 = story_util:set_order(Story, 30)
    , etap:is(story_util:order(Story2), 30, "successfully set order")
.

-plan(2).
test_sort_by_order_desc() ->
    List = make_ord_list([6,3,9,1])
    , [SortedAscFirst | _]  = story_util:sort({ord, desc}, List)
    , etap:is(story_util:order(SortedAscFirst), 9,
        "the last shall be first for desc")
    , [SortedAscLast | _]   = lists:reverse(story_util:sort({ord, desc}, List))
    , etap:is(story_util:order(SortedAscLast), 1,
        "the first shall be last for desc sort")
.

-plan(2).
test_sort_by_order_asc() ->
    List = make_ord_list([6,3,9,1])
    , [SortedAscFirst | _]  = story_util:sort({ord, asc}, List)
    , etap:is(story_util:order(SortedAscFirst), 1,
        "the first shall be first for asc sort")
    , [SortedAscLast | _]   = lists:reverse(story_util:sort({ord, asc}, List))
    , etap:is(story_util:order(SortedAscLast), 9,
        "the last shall be last for asc sort")
.

make_ord_story(N) -> #stories{meta=[{ord, N}]} .

make_ord_list([]) ->
    [];
make_ord_list(L) ->
    [N | T] = L
    , [make_ord_story(N) | make_ord_list(T)]
.

