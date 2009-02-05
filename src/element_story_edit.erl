-module(element_story_edit).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("elements.hrl").

render(ControlId, Record) ->
    %% TODO(jwall): change to temp ids wf:temp_id()
    PanelId        = wf:temp_id()
    , Name           = Record#story_edit.story_name
    , Desc           = Record#story_edit.desc
    , StoryPoints    = Record#story_edit.sp
    , Panel = #panel{ id=PanelId
                    , body=[
                        #inplace_textbox{text=Name}, #br{}
                        , #inplace_textbox{text=Desc}, #br{}
                        , #inplace_textbox{text=StoryPoints}, #br{}
                    ]
    }
    , element_panel:render(ControlId, Panel).

