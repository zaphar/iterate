-module(iterate_db).

-export([backlogs/0, stories/1]).

backlogs() ->
    ["Default", "Mine"].

stories("Default") ->
    ["Story One"];
stories("Mine") ->
     ["add and delete stories"
      , "add and delete backlogs"
      , "storage layer for backlogs and data"
      , "drag drop stories to backlogs"
      , "backlog filter and search"];
stories(B) when is_list(B) ->
    [].
