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
    [#panel{ id=PanelId, actions=#event{type=click
                , delegate=element_story_panel
                , postback=?SHOW_STORIES(iteration, Name)
                , actions="$('.backlog_element.selected')"
                    ++ ".removeClass('selected', 500);"
                    ++ "$(obj('me')).addClass('selected', 250)"
             }
             , body=[#label{ id=Name ++ "_name", text=Name}
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
             ]
    }
    , #panel{id=Name ++ "_target"}]
.

%% showing iteration info
event(?UPDATE_B_EL(Name, Id)) ->
    io:format("updating iteration widget ~s for: ~s~n", [Id, Name])
    , case iterate_db:iteration(?Q_ITERATION(Name)) of
        [_ | []] ->
            wf:update(Id, body(Name, Id))
    end;
event(?SHOW_B_EL(Name, Id)) ->
    io:format("showing edit widget for: ~s~n", [Name])
    , case iterate_db:iteration(?Q_ITERATION(Name)) of
        [B | []] ->
            wf:update(Name ++ "_target",
                #iteration_edit{ iteration_id=Name, el_id=Id, 
                    desc=B#iterations.desc })
    end;
event(?DELETE_B_EL(Name, Id)) ->
    wf:flash(io_lib:format("hiding element: ~p~n", [Id]))
    , iterate_stats:record(iteration, ?DELETE_STAT(Name))
    , io:format("hiding element: ~p~n", [Id])
    , Result = iterate_db:iteration(?DELITER(Name))
    , io:format("Result: ~p~n", [Result])
    , wf:wire(Id, #hide{ effect=slide, speed=500 })
    , event(?REMOVE_B_EL(Name, Id));
event(?REMOVE_B_EL(Name, _Id)) ->
    wf:update(Name ++ "_target", "");
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
.

