-module(element_report_panel).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("stats.hrl").
-include("elements.hrl").

-import(iterate_report, [completion_for_last_week/1]).

reflect() -> record_info(fields, report_panel).

render() ->
    wf:render(#report_panel{})
.

render(ControlId, _Record) ->
    PanelId = wf:temp_id()
    , CChart = build_completion_chart({iteration, "rc2 sprint"})
    , Panel = #panel{id=PanelId, body=["Completion Burnup chart"
        , CChart]}
    , element_panel:render(ControlId, Panel)
.

build_completion_chart({Type, Name}) ->
    TS = completion_for_last_week({Type, Name})
    % TODO(jwall): make this into a widget and blog about it
    %, Length = length(TS)
    %, Title = wf:f("~p Completion Burndown for Last 7 days", [Name])
    , Width = 600
    , Height = 300
    , MinX = date_util:date_to_epoch(date_util:subtract(date(), {days, 7})) * 1000 
    , MaxX = date_util:date_to_epoch(date_util:add(date(), {days, 1})) * 1000
    , YTicks = 5
    , MinY = 0
    , MaxY = 100
    %, BGColor = "white"
    %, ChartColor = "grey"
    , Data = [[V#tsentry.epoch, V#tsentry.value] || V <- TS]
    , TargetId = wf:temp_id()
    , GraphId = wf:temp_id()
    , ScriptId = wf:temp_id()
    %% TODO(jwall): legend support
    %% TODO(jwall): colors
    %% TODO(jwall): add interactive support (zoom, tooltip, select)
    , Script = wf:f("<script id=~p language='javascript', type='text/javascript'>"
        ++ "$(function() { ~n"
        ++ "var d = ~w;~n"
        ++ "$.plot($('#' + ~p), [d], {xaxis: { mode: 'time',"
        ++ " ticks: 7,"
        ++ " min: (new Date(~p).getTime()),"
        ++ " max: (new Date(~p).getTime())"
        ++ " }, "
        ++ "yaxis: {ticks: ~p,"
        ++ "min: ~p,"
        ++ "max: ~p,"
        ++ "}"
        ++ "});~n});</script>"
        , [ScriptId, Data, TargetId, MinX, MaxX, YTicks, MinY, MaxY])
    , Panel = #panel{id=TargetId, style=wf:f("width:~ppx;height:~ppx", [Width, Height])}
    , #panel{id=GraphId, body=[Panel, Script]}
.
