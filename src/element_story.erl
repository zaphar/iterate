-module(element_story).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("elements.hrl").

render(ControlId, Record) ->
    %% TODO(jwall): change to temp ids wf:temp_id()
    PanelId = wf:temp_id(),
    Name    = Record#story.story_name,
    Panel = #panel{ id=PanelId,
                    body=[
                        Name
                        , " "
                        , #link{text="edit"
                            , actions=#event{ type=click, delegate=?MODULE
                                            , postback={show, {story, Name}}
                            }
                        }
                        , #panel{id=Name ++ "_target"}
                    ]
    },
    element_panel:render(ControlId, Panel).

%% showing backlog info
event({show, {story, Name}}) ->
    wf:update(Name ++ "_target",
        #story_edit{story_name=Name, desc="A description", sp="3"});
event({remove, {story, Name}}) ->
    wf:update(Name ++ "_target", "");
event(_) -> ok.
