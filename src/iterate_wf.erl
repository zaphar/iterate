-module(iterate_wf).

-include("events.hrl").
-include("iterate_records.hrl").
-include("stats.hrl").

-export([create_backlog/1, delete_backlog/1]).
-export([search_for_backlog/1]).

-export([move_story_to_backlog/2, move_story_to_iteration/2]).
-export([update_story_completion/2]).

%% TODO(jwall): should these be broken into story_wf, backlog_wf and so on?

%% Backlog APIs

create_backlog(Name) ->
   Result = iterate_db:backlog({new, #backlogs{backlog_name=Name}})
   , iterate_stats:record(backlog, ?CREATE_STAT(Name))
   , Result
.

delete_backlog(Name) ->
    Records = iterate_db:backlog({qry, Name})
    , io:format("found: ~p~n", [Records])
    , [Record | []] = Records
    , Result = iterate_db:backlog({delete, Record})
    , iterate_stats:record(backlog, ?DELETE_STAT(Name))
    , io:format("Result: ~p~n", [Result])
    , ok
.

%update_backlog(Backlog) ->
%.

search_for_backlog("desc:" ++ Term) ->
    iterate_db:backlog(?Q_SEARCH_BACKLOG(desc, Term));
search_for_backlog("id:" ++ Term) ->
    search_for_backlog(Term);
search_for_backlog(Term) ->
    iterate_db:backlog(?Q_SEARCH_BACKLOG(id, Term))
.

%get_backlogs() ->
%.

%% Story APIs

%% TODO(jwall): write tests for this :-)
move_story_to_backlog(Story, Backlog) ->
    [StoryRecord | []] = iterate_db:story(?Q_STORY(Story))
    , OldBacklog = StoryRecord#stories.backlog
    %% TODO(jwall): figure out if the old location was iteration or backlog
    , iterate_stats:record(story, ?MOVE_STAT(Story, Backlog, OldBacklog))
    , io:format("found story: ~p ~n", [StoryRecord])
    , NewStory = story_util:set_backlog(StoryRecord, Backlog)
    , io:format("changed story to: ~p ~n", [NewStory])
    , iterate_db:story({update, NewStory})
    , {old_backlog, OldBacklog}
.

move_story_to_iteration(_Story, _Iteration) ->
    ok
.

update_story_completion(Name, Percent) ->
    [Original] = iterate_db:story(?Q_STORY(Name))
    , New = story_util:set_percent(Original, Percent)
    , iterate_stats:record(story, 
        ?CHANGE_STAT(Name, percent, Percent))
    , iterate_db:story({update, New})
.

