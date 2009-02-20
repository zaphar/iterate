-module(element_backlog).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("elements.hrl").
-include("macros.hrl").
-include("iterate_records.hrl").

render(_ControlId, Record) ->
    Name    = Record#backlog.backlog_name,
    PanelId = wf:temp_id(),
    Panel = #delegated_droppable{ tag={Name, {delegate, ?MODULE}},
        body=body(Name, PanelId) },
    io:format("the panel id for ~s is ~s~n", [Name, PanelId]),
    element_delegated_droppable:render(PanelId, Panel).

body(Name, PanelId) ->
    [#panel{ id=PanelId, actions=#event{type=click
                   , delegate=element_story_panel
                   , postback=?SHOW_STORIES(Name)
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
    , #panel{id=Name ++ "_target"}].

%% showing backlog info
%% TODO(jwall): need to hide edit button for this?
event(?UPDATE_B_EL(Name, Id)) ->
    io:format("updating backlog widget ~s for: ~s~n", [Id, Name])
    , case iterate_db:backlog({qry, Name}) of
        [_ | []] ->
            wf:update(Id, body(Name, Id))
    end;
event(?SHOW_B_EL(Name, Id)) ->
    io:format("showing edit widget for: ~s~n", [Name])
    , case iterate_db:backlog({qry, Name}) of
        [B | []] ->
            wf:update(Name ++ "_target",
                #backlog_edit{ backlog_id=Name, el_id=Id, desc=B#backlogs.desc })
    end;
event(?DELETE_B_EL(Name, Id)) ->
    io:format("looking up: ~p~n", [Name])
    , Records = iterate_db:backlog({qry, Name})
    , io:format("found: ~p~n", [Records])
    , wf:flash(io_lib:format("hiding element: ~p~n", [Id]))
    , io:format("hiding element: ~p~n", [Id])
    , [Record | []] = Records
    , Result = iterate_db:backlog({delete, Record})
    , io:format("Result: ~p~n", [Result])
    , wf:wire(Id, #hide{ effect=slide, speed=500 })
    , event(?REMOVE_B_EL(Name, Id));
event(?REMOVE_B_EL(Name, _Id)) ->
    wf:update(Name ++ "_target", "");
event(Event) -> 
    io:format("received event: ~p~n", [Event])
.

%% move a story to a backlog
drop_event(Story, Backlog) ->
    io:format("received event: ~p -> ~p~n", [Story, Backlog])
    , [StoryRecord | []] = iterate_db:story(?Q_STORY(Story))
    , io:format("found story: ~p ~n", [StoryRecord])
    , OldBacklog = StoryRecord#stories.backlog
    , io:format("changed story to: ~p ~n", [StoryRecord#stories{backlog=Backlog}])
    , iterate_db:story({update, StoryRecord#stories{backlog=Backlog}})
    , element_story_panel:event(?SHOW_STORIES(OldBacklog))
    , ok
.

