-module(element_backlog_panel).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("elements.hrl").
-include("iterate_records.hrl").

render(ControlId, Record) ->
    %% TODO(jwall): change to temp ids wf:temp_id()
    PanelId = "backlog_panel"
    , Data    = case Record#backlog_panel.data of
        undefined ->
            [];
        D when is_list(D) ->
            D
    end
    , Panel = #rounded_panel{ id=PanelId, body=body(Data) }
    , element_rounded_panel:render(ControlId, Panel).

body(Data) ->
    #list{ body=backlogs(Data) }.

%% generate our backlog list
backlogs([]) ->
    [];
backlogs([H|T]) ->
    Name = H#backlogs.backlog_name
    , [ #listitem{ body=#backlog{backlog_name=Name} } | backlogs(T) ].

%% showing backlog info
event({show, {backlog, Name}}) ->
    wf:update(Name ++ "_target",
        #backlog_edit{backlog_id=Name, desc="A description"});
event({remove, {backlog, Name}}) ->
    wf:update(Name ++ "_target", "");
event(_) -> ok.
