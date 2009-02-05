-module(element_backlog_edit).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("elements.hrl").

render(ControlId, Record) ->
    %% TODO(jwall): change to temp ids wf:temp_id()
    Name = Record#backlog_edit.backlog_id ++ "_BacklogEditBox",
    ButtonId = Name ++ "CloseButton",
    Button = #button{ id=ButtonId, text="close"},
    Panel = #panel{ id=Name,
        body=[
           #my_inplace_textbox{ delegate=?MODULE
                , text=Record#backlog_edit.backlog_id }, #br{}
           , #my_inplace_textbox{ delegate=?MODULE
                , text=Record#backlog_edit.desc }, #br{}
           , Button
        ]
    },
    wf:wire(ButtonId, 
        #event{ type=click, delegate=element_backlog, 
            postback={remove, {backlog, Record#backlog_edit.backlog_id}}
        }
    ),
    element_panel:render(ControlId, Panel).

%% TODO(jwall): bind this to do actual work
inplace_textbox_event(_Tag, Value) ->
    Value.

