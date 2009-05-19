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

backlog_tags(Name) ->
    [ binary_to_list(B) || B <- string_utils:flatten_string_list([ get_story_tags(S) 
            || S <- get_backlog_stories(Name)]), B /= <<"tag">> ]
.

get_backlogs() -> iterate_db:backlogs().

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
            log_story_stats(Story)
            , Story
    end
.

get_story(Name) ->
    iterate_db:story(?Q_STORY(Name))
.

get_story_tags(S) when is_record(S, stories) ->
    get_story_tags(S#stories.story_name);
get_story_tags(Name) ->
    {atomic, TagList} = iterate_db:tags(?Q_TAGS(story, Name))
    , case TagList of
        [] ->
            [list_to_binary(?TVALUE(T)) || T <- [?STAG(Name, "tag")] ];
        List ->
            [list_to_binary(?TVALUE(T)) || T <- List]
    end
.

update_story_tags(For, TagList) ->
    iterate_db:tag_delete(story, For)
    , [iterate_db:tags(?NEWTAG(story, For, T)) || T <- TagList]
    , TagList
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
    , log_iteration_stats(story_util:iteration(StoryRecord))
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
    , log_iteration_stats(story_util:iteration(StoryRecord))
    %% we need to log the iterations new completion also
    , log_iteration_stats(story_util:iteration(NewStory))
    , {old_location, OldBacklog}
.

update_story_completion(Name, Percent) ->
    [Original] = iterate_db:story(?Q_STORY(Name))
    , New = story_util:set_percent(Original, Percent)
    , Resulting = iterate_db:story({update, New})
    %% if story is in an iteration then we need to log iteration's new
    %% aggregate completion
    , log_story_stats(New)
    , log_iteration_stats(story_util:iteration(New))
    , Resulting
.

update_story_points(Story, Value) 
    when is_integer(Value) and is_record(Story, stories) ->
        Updated = Story#stories{sp=Value}
        , Resulting = iterate_db:story({update, Updated})
        , log_story_stats(Story)
        , log_iteration_stats(story_util:iteration(Story))
        , Resulting;
update_story_points(Name, Value) when is_list(Value) ->
    update_story_points(Name, list_to_integer(Value));
update_story_points(Name, Value) when is_integer(Value) ->
    [Story] = get_story(Name)
    , update_story_points(Story, Value)
.

log_story_stats(S) ->
    iterate_stats:record(story
        , ?CHANGE_STAT(S#stories.story_name, sp, S#stories.sp))
    , iterate_stats:record(story
        , ?CHANGE_STAT(S#stories.story_name, percent, story_util:completion(S)))
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

log_iteration_stats(Iter) ->
    log_iteration_completion(Iter)
    , log_iteration_story_points(Iter)
.

log_iteration_completion(0) ->
    ok;
log_iteration_completion(Iter) ->
    Agg = iteration_completion(Iter)
    , iterate_stats:record(iteration, ?CHANGE_STAT(Iter, percent, Agg))
.

log_iteration_story_points(Iter) ->
    {Complete, Incomplete} = iteration_story_points(Iter)
    , iterate_stats:record(iteration, ?CHANGE_STAT(Iter, incomplete_sp, Incomplete))
    , iterate_stats:record(iteration, ?CHANGE_STAT(Iter, complete_sp, Complete))
.

iteration_completion(Name) ->
    story_util:aggregate_completion(get_iteration_stories(Name))
.

iteration_story_points(Name) ->
    {Complete, Incomplete} = lists:foldl(
        fun(S, {CompAgg, IncAgg}) ->
            case story_util:is_complete(S) of
                true ->
                    {[S | CompAgg], IncAgg};
                false ->
                    {CompAgg, [S | IncAgg]}
            end
        end, {[], []}, get_iteration_stories(Name))
    , {story_util:aggregate_story_points(Complete)
       , story_util:aggregate_story_points(Incomplete)}
.

iteration_tags(Name) ->
    [ binary_to_list(B) || B <- string_utils:flatten_string_list([ get_story_tags(S) 
            || S <- get_iteration_stories(Name)]), B /= <<"tag">> ]
.

get_iteration(Name) ->
    case iterate_db:iteration(?Q_ITERATION(Name)) of
        [Iteration] ->
            {ok, Iteration};
        [] ->
            {abort, no_such_iteration}
    end
.

%% Misc

get_default_backlog() ->
    {backlog, "Default"}
.

stop_working_in({Type, Name}) ->
    case working_in() of
        {Type, Name} ->
            stop_working_in();
        _ ->
            {Type, Name}
    end
.

stop_working_in() ->
        wf_session:session(working_in, undefined)
        , working_in()
.

working_in({Type, Name}) 
    when Type == iteration orelse Type == backlog ->
        session(working_in, {Type, Name})
.

working_in() ->
    case session(working_in) of
        undefined ->
            get_default_backlog();
        {'EXIT', _} ->
            get_default_backlog();
        {Type, Name} ->
            {Type, Name}
    end
.

session(Key, Val) ->
    catch wf_session:session(Key, Val)
.

session(Key) ->
    catch wf_session:session(Key)
.

