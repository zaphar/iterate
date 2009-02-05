-module(element_story_edit).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("elements.hrl").

render(ControlId, Record) ->
    PanelId        = wf:temp_id()
    , ButtonId       = wf:temp_id()
    , Name           = Record#story_edit.story_name
    , Desc           = Record#story_edit.desc
    , StoryPoints    = Record#story_edit.sp
    , Panel = #panel{ id=PanelId
                    , body=[
                        #inplace_textbox{text=Name}, #br{}
                        , #inplace_textbox{text=Desc}, #br{}
                        , #inplace_textbox{text=StoryPoints}, #br{}
                        , #button{id=ButtonId, text="close"
                            , actions=#event{type=click
                                             , delegate=element_story
                                             , postback={remove, {story, Name}}}
                        }
                    ]
    }
    , element_panel:render(ControlId, Panel).

