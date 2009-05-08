-module(element_iteration).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("stats.hrl").
-include("elements.hrl").
-include("events.hrl").
-include("iterate_records.hrl").

render(_ControlId, Record) ->
    Name    = Record#iteration.iteration_name
    , PanelId = wf:temp_id()
    , Panel = #delegated_droppable{ id=PanelId
        , class="panel_element backlog_element" ++ selection_class(Name)
        , hover_class=drop_hover
        , tag={Name, {delegate, ?MODULE}}
        , body=body(Name, PanelId) }
    , io:format("the panel id for ~s is ~s~n", [Name, PanelId])
    , element_delegated_droppable:render(PanelId, Panel)
.

is_working_in({iteration, Name}, Me) ->
    Me == Name;
is_working_in(_, _) ->
    false
.

selection_class(Me) ->
    case is_working_in(wf_session:session(working_in), Me) of
        true ->
            " selected";
        false ->
            ""
    end
.

body(Name, PanelId) ->
    Title = wf:f("~s ~.1f%"
        , [Name, iterate_wf:iteration_completion(Name)])
    , [#panel{ id=PanelId, actions=#event{type=click
                , delegate=element_story_panel
                , postback=?SHOW_STORIES(iteration, Name)
                , actions="$('.backlog_element.selected')"
                    ++ ".removeClass('selected', 500);"
                    ++ "$(obj('me')).addClass('selected', 250)"
             }
             , body=[#label{ id=Name ++ "_name", text=Title}
                 , " " , #link{text="edit"
                         , actions=#event{type=click, delegate=?MODULE
                             , postback=?SHOW_B_EL(Name, PanelId)
                         }
                 }
                 , " " , #link{text="delete"
                         , actions=#event{type=click, delegate=?MODULE
                             , postback=?DELETE_B_EL(Name, PanelId)
                         }
                 }
                 , " " , #link{text="close"
                         , actions=#event{type=click, delegate=?MODULE
                             , postback=?CLOSE_I_EL(Name, PanelId)
                         }
                 }
             ]
    }
    , #panel{id=Name ++ "_target"}]
.

%% showing iteration info
event(?UPDATE_B_EL(Name, Id)) ->
    case iterate_db:iteration(?Q_ITERATION(Name)) of
        [_ | []] ->
            wf:update(Id, body(Name, Id))
    end;
event(?SHOW_B_EL(Name, Id)) ->
    case iterate_db:iteration(?Q_ITERATION(Name)) of
        [B | []] ->
            wf:update(Name ++ "_target",
                #iteration_edit{ iteration_id=Name, el_id=Id, 
                    desc=B#iterations.desc })
    end;
event(?DELETE_B_EL(Name, Id)) ->
    iterate_wf:delete_iteration(Name)
    , wf:wire(Id, #hide{ effect=slide, speed=500 })
    , event(?REMOVE_B_EL(Name, Id));
event(?CLOSE_I_EL(Name, Id)) ->
    iterate_wf:close_iteration(Name)
    , wf:wire(Id, #hide{ effect=slide, speed=500 })
    , event(?REMOVE_B_EL(Name, Id));
event(?REMOVE_B_EL(Name, _Id)) ->
    iterate_wf:stop_working_in()
    , {Type, BName} = iterate_wf:working_in()
    , element_iteration_panel:event(?REFRESH(_Id))
    , element_story_panel:event(?SHOW_STORIES(Type, BName))
event(Event) -> 
    io:format("received event: ~p~n", [Event])
.

%% move a story to a backlog
drop_event(Story, Iteration) ->
    io:format("received event: ~p -> ~p~n", [Story, Iteration])
    , [StoryRecord | []] = iterate_db:story(?Q_STORY(Story))
    , Backlog = StoryRecord#stories.backlog
    , iterate_stats:record(story
        , ?MOVE_STAT(Story, Iteration, Backlog))
    , io:format("found story: ~p ~n", [StoryRecord])
    , NewStory = story_util:set_iteration(StoryRecord, Iteration)
    , io:format("changed story to: ~p ~n", [NewStory])
    , iterate_db:story(?Q_UPDATE_STORY(NewStory))
    , wf:flash(
        io_lib:format("Took on Story: ~p in Iteration: ~p", [Story, Iteration]))
    , {Type, _Name} = story_util:get_type(StoryRecord)
    , element_story_panel:event(?SHOW_STORIES(Type, Backlog))
    , element_iteration_panel:event(?REFRESH(bogus_id))
.

get_iter(Name) ->
    case iterate_db:iteration(?Q_ITERATION(Name)) of
        [Iteration] ->
            {ok, Iteration};
        [] ->
            {abort, no_such_iteration}
    end
.

