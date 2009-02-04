-module(element_backlog_el).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("elements.hrl").

render(ControlId, Record) ->
    Name = Record#backlog_el.backlog_id ++ "_BacklogEditBox",
    ButtonId = Name ++ "CloseButton",
    Button = #button{ id=ButtonId, text="close"},
    Panel = #panel{ id=Name,
        body=[
           #inplace_textbox{ text=Record#backlog_el.backlog_id }, #br{}
           , #inplace_textbox{ text=Record#backlog_el.desc }, #br{}
           , Button
        ]
    },
    wf:wire(ButtonId, 
        #event{ type=click, delegate=web_backlogs, 
            postback={remove, {backlog, Record#backlog_el.backlog_id}}
        }
    ),
    element_panel:render(ControlId, Panel).

