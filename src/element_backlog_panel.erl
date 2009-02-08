-module(element_backlog_panel).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("elements.hrl").

render(ControlId, Record) ->
    %% TODO(jwall): change to temp ids wf:temp_id()
    PanelId = wf:temp_id()
    , Data    = case Record#backlog_panel.data of
        undefined ->
            [];
        D when is_list(D) ->
            D
    end
    , Panel = #rounded_panel{ id=PanelId,
        body=#list{ body=backlogs(Data) }
    }
    , element_rounded_panel:render(ControlId, Panel).

%% generate our backlog list
backlogs([]) ->
    [];
backlogs([H|T]) ->
    [ #listitem{ body=#backlog{backlog_name=H} } | backlogs(T) ].
    

%% showing backlog info
event({show, {backlog, Name}}) ->
    wf:update(Name ++ "_target",
        #backlog_edit{backlog_id=Name, desc="A description"});
event({remove, {backlog, Name}}) ->
    wf:update(Name ++ "_target", "");
event(_) -> ok.
