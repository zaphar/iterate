-module(iterate_wf).
-compile(export_all).

-include("events.hrl").
-include("iterate_records.hrl").
-include("stats.hrl").


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
    , Result
.

search_for_backlog("desc:" ++ Term) ->
    iterate_db:backlog(?Q_SEARCH_BACKLOG(desc, Term));
search_for_backlog("id:" ++ Term) ->
    iterate_db:backlog(?Q_SEARCH_BACKLOG(id, Term));
search_for_backlog(Term) ->
    iterate_db:backlog(?Q_SEARCH_BACKLOG(all, Term))
.

%% Story APIs

create_story(Title) ->
    #stories{story_name=iterate_db:uuid(), story_title=Title}
.

create_story_for(Title, {backlog, Backlog}) ->
    iterate_stats:record(story, ?CREATE_STAT(Title))
    , Story = story_util:set_backlog(create_story(Title), Backlog)
    , iterate_db:story({new, Story})
    , commit_story(Story);
create_story_for(Title, {iteration, Iteration}) ->
    Story = story_util:set_iteration(create_story(Title), Iteration)
    , commit_story(Story)
.

commit_story(Story) ->
    case iterate_db:story({new, Story}) of
        {error, Msg} ->
            throw(Msg);
        {atomic, ok} ->
            Story
    end
.

get_story(Name) ->
    iterate_db:story(?Q_STORY(Name))
.

get_iteration_stories(Name) ->
    iterate_db:story(?Q_ITERATION_STORY(Name))
.

get_backlog_stories(Name) ->
    iterate_db:story(?Q_BACKLOG_STORY(Name))
.

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
    %% if the old Story was in an iteration then we need to 
    %% log the new iteration percent completion
    , log_iteration_completion(story_util:iteration(StoryRecord))
    , {old_location, OldBacklog}
.

move_story_to_iteration(Story, Iteration) ->
    [StoryRecord | []] = iterate_db:story(?Q_STORY(Story))
    , OldBacklog = StoryRecord#stories.backlog
    %% TODO(jwall): figure out if the old location was iteration or backlog
    , iterate_stats:record(story, ?MOVE_STAT(Story, Iteration, OldBacklog))
    , io:format("found story: ~p ~n", [StoryRecord])
    , NewStory = story_util:set_iteration(StoryRecord, Iteration)
    , io:format("changed story to: ~p ~n", [NewStory])
    , iterate_db:story({update, NewStory})
    %% if the old Story was in an iteration then we need to 
    %% log the new iteration percent completion
    , log_iteration_completion(story_util:iteration(StoryRecord))
    %% we need to log the iterations new completion also
    , log_iteration_completion(story_util:iteration(NewStory))
    , {old_location, OldBacklog}
.

update_story_completion(Name, Percent) ->
    [Original] = iterate_db:story(?Q_STORY(Name))
    , New = story_util:set_percent(Original, Percent)
    , iterate_stats:record(story, 
        ?CHANGE_STAT(Name, percent, Percent))
    , iterate_db:story({update, New})
    %% if story is in an iteration then we need to log iteration's new
    %% aggregate completion
    , log_iteration_completion(story_util:iteration(New))
.

log_iteration_completion(0) ->
    ok;
log_iteration_completion(Iter) ->
    Agg = iteration_completion(Iter)
    , iterate_stats:record(iteration, ?CHANGE_STAT(Iter, percent, Agg))
.

%% Iteration APIs

create_iteration(Name) ->
    iterate_stats:record(iteration, ?CREATE_STAT(Name))
    , iterate_db:iteration(?NEWITER(Name, "fill in description here"))
.

close_iteration(Name) ->
    iterate_stats:record(iteration, ?CLOSE_STAT(iteration, Name))
    , {ok, Iter} = get_iteration(Name) 
    , iterate_db:iteration(?UPDATEITER(iteration_util:close(Iter)))
.

delete_iteration(Name) ->
    iterate_stats:record(iteration, ?DELETE_STAT(Name))
    , iterate_db:iteration(?DELITER(Name))
.

search_for_iteration(_Crit) ->
    ok
.

get_started_iterations() ->
    iterate_db:iterations(started)
.

iteration_completion(Name) ->
    story_util:aggregate_completion(get_iteration_stories(Name))
.

get_iteration(Name) ->
    case iterate_db:iteration(?Q_ITERATION(Name)) of
        [Iteration] ->
            {ok, Iteration};
        [] ->
            {abort, no_such_iteration}
    end
.
