-module(element_backlog).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("elements.hrl").

render(ControlId, Record) ->
    %% TODO(jwall): change to temp ids wf:temp_id()
    PanelId = wf:temp_id(),
    Name    = Record#backlog.backlog_name,
    Panel = #panel{ id=PanelId,
                    body=[
                        #panel{ body=Name
                                , actions=#event{type=click
                                       , delegate=web_index
                                       , postback={show, {stories, Name}}
                                }
                        }, " "
                        , #link{text="edit"
                                , actions=#event{type=click, delegate=?MODULE
                                    , postback={show, {backlog, Name}}
                                }
                        }
                        , #panel{id=Name ++ "_target"}
                    ]
    },
    element_panel:render(ControlId, Panel).

%% showing backlog info
event({show, {backlog, Name}}) ->
    wf:update(Name ++ "_target",
        #backlog_edit{backlog_id=Name, desc="A description"});
event({remove, {backlog, Name}}) ->
    wf:update(Name ++ "_target", "");
event(_) -> ok.
