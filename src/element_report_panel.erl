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
    , CChart = build_completion_chart(wf_session:session(working_in))
    , Panel = #panel{id=PanelId, body=["Completion Burnup chart"
        , CChart]}
    , element_panel:render(ControlId, Panel)
.

build_completion_chart(undefined) ->
    "no reports selected";
build_completion_chart({Type, Name}) ->
    TS = completion_for_last_week({Type, Name})
    % TODO(jwall): make this into a widget and blog about it
    %, Length = length(TS)
    %, Title = wf:f("~p Completion Burndown for Last 7 days", [Name])
    , Width = 300
    , Height = 150
    , MinX = date_util:date_to_epoch(date_util:subtract(date(), {days, 7})) * 1000 
    , MaxX = date_util:date_to_epoch(date_util:add(date(), {days, 1})) * 1000
    , YTicks = 5
    , MinY = 0
    , MaxY = 100
    , Data = {Name, [[V#tsentry.epoch, V#tsentry.value] || V <- TS]}
    , #flot_chart{width=Width, height=Height
        , minx=MinX, maxx=MaxX, yticks=YTicks, xticks=8
        , miny=MinY, maxy=MaxY, values=Data}
.

build_sp_chart(undefined) ->
    "no reports selected";
build_sp_chart({Type, Name}) ->
    TS = completion_for_last_week({Type, Name})
    % TODO(jwall): make this into a widget and blog about it
    %, Length = length(TS)
    %, Title = wf:f("~p Completion Burndown for Last 7 days", [Name])
    , Width = 300
    , Height = 150
    , MinX = date_util:date_to_epoch(date_util:subtract(date(), {days, 7})) * 1000 
    , MaxX = date_util:date_to_epoch(date_util:add(date(), {days, 1})) * 1000
    , YTicks = 5
    , MinY = 0
    , MaxY = 100
    , Data = {Name, [[V#tsentry.epoch, V#tsentry.value] || V <- TS]}
    , #flot_chart{width=Width, height=Height
        , minx=MinX, maxx=MaxX, yticks=YTicks, xticks=8
        , miny=MinY, maxy=MaxY, values=Data}
.

