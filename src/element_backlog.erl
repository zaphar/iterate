-module(element_backlog).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("elements.hrl").
-include("iterate_records.hrl").

render(ControlId, Record) ->
    %% TODO(jwall): change to temp ids wf:temp_id()
    PanelId = wf:temp_id(),
    Name    = Record#backlog.backlog_name,
    Panel = #panel{ id=PanelId,
                    body=[
                        #panel{ actions=#event{type=click
                                       , delegate=element_story_panel
                                       , postback={show, {stories, Name}}
                                }
                                , body=[Name, " "
                                , #link{text="edit"
                                        , actions=#event{type=click, delegate=?MODULE
                                            , postback={show, {backlog, Name}}
                                        }
                                }]
                        }
                        , #panel{id=Name ++ "_target"}
                    ]
    },
    element_panel:render(ControlId, Panel).

%% showing backlog info
event({show, {backlog, Name}}) ->
    case iterate_db:backlog({qry, Name}) of
        %%{error, Msg} ->
            %% what do I do for this one?
        [B | []] ->
            wf:update(Name ++ "_target",
                #backlog_edit{ backlog_id=Name, desc=B#backlogs.desc })
    end;
event({remove, {backlog, Name}}) ->
    wf:update(Name ++ "_target", "");
event(_) -> ok.
