-module(element_story).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("macros.hrl").
-include("elements.hrl").

render(ControlId, Record) ->
    %% TODO(jwall): change to temp ids wf:temp_id()
    PanelId = wf:temp_id(),
    Name    = Record#story.story_name,
    Panel = #draggable{ tag=Name, body=#panel{ id=PanelId,
                    body=[
                        Name
                        , " "
                        , #link{text="edit"
                            , actions=#event{ type=click, delegate=?MODULE
                                            , postback=?SHOW_S_EL(Name)
                            }
                        }, " "
                        , #link{text="delete"
                            , actions=#event{ 
                                type=click
                                , delegate=?MODULE
                                , postback=?DELETE_S_EL(Name, PanelId)
                            }
                        }
                        , #panel{id=Name ++ "_target"}
                    ]
    }},
    element_draggable:render(ControlId, Panel).

event(?SHOW_S_EL(Name)) ->
    wf:update(Name ++ "_target",
        #story_edit{story_name=Name, desc="A description", sp="3"});
event(?REMOVE_S_EL(Name)) ->
    wf:update(Name ++ "_target", "");
event(?DELETE_S_EL(Name, Id)) ->
    Records = iterate_db:story(?Q_STORY(Name))
    , [Record | []] = Records
    , _Result = iterate_db:story({delete, Record})
    , wf:wire(Id, #hide{ effect=slide, speed=500 })
    , event(?REMOVE_S_EL(Name))
    , ok;
event(_) -> 
    ok
.

