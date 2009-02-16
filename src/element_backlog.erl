-module(element_backlog).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("elements.hrl").
-include("macros.hrl").
-include("iterate_records.hrl").

render(ControlId, Record) ->
    PanelId = wf:temp_id(),
    Name    = Record#backlog.backlog_name,
    Panel = #panel{ id=PanelId, body=body(Name, PanelId) },
    io:format("the panel id for ~s is ~s~n", [Name, PanelId]),
    element_panel:render(ControlId, Panel).

body(Name, PanelId) ->
    [#panel{ actions=#event{type=click
                   , delegate=element_story_panel
                   , postback=?SHOW_B_EL(Name, PanelId)
            }
            , body=[#label{ id=Name ++ "_name", text=Name}
            , " " , #link{text="edit"
                    , actions=#event{type=click, delegate=?MODULE
                        , postback=?SHOW_B_EL(Name, PanelId)
                    }
            }]
    }
    , #panel{id=Name ++ "_target"}].

%% showing backlog info
%% TODO(jwall): need to hide edit button for this?
event(?UPDATE_B_EL(Name, Id)) ->
    io:format("updating backlog widget ~s for: ~s~n", [Id, Name])
    , case iterate_db:backlog({qry, Name}) of
        %%{error, Msg} ->
            %% what do I do for this one?
        [_ | []] ->
            wf:update(Id, body(Name, Id))
    end;
event(?SHOW_B_EL(Name, Id)) ->
    io:format("showing edit widget for: ~s~n", [Name])
    , case iterate_db:backlog({qry, Name}) of
        %%{error, Msg} ->
            %% what do I do for this one?
        [B | []] ->
            wf:update(Name ++ "_target",
                #backlog_edit{ backlog_id=Name, el_id=Id, desc=B#backlogs.desc })
    end;
%% TODO(jwall): need to show edit button again
event(?REMOVE_B_EL(Name, _Id)) ->
    wf:update(Name ++ "_target", "");
event(_) -> ok.

