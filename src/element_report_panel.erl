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
    , Panel = #panel{id=PanelId, body=["Completion/Story Point chart"
        , CChart]}
    , element_panel:render(ControlId, Panel)
.

build_completion_chart(undefined) ->
    "no reports selected";
build_completion_chart({Type, Name}) ->
    TS1 = iterate_report:stats(incomplete_sp, Type, Name)
    , TS2 = iterate_report:stats(complete_sp, Type, Name)
    , TS3 = completion_for_last_week({Type, Name})
    % TODO(jwall): make this into a widget and blog about it
    , {Complete, Incomplete} = iterate_wf:iteration_story_points(Name)
    , Width = 400
    , Height = 150
    , MinX = date_util:date_to_epoch(date_util:subtract(date(), {days, 7})) * 1000 
    , MaxX = date_util:date_to_epoch(date_util:add(date(), {days, 2})) * 1000
    , YTicks = 5
    , MinY = 0
    , MaxY = 100
    , MinY2 = 0
    , MaxY2 = Complete + Incomplete 
    , Data = [{"Completion", [[V#tsentry.epoch, V#tsentry.value] || V <- TS3]}
        , {"SP burndown", [[V#tsentry.epoch, V#tsentry.value] || V <- TS1], {yaxis, 2}}
        , {"SP burnup", [[V#tsentry.epoch, V#tsentry.value] || V <- TS2], {yaxis, 2}}]
    , #flot_chart{width=Width, height=Height, modex="time"
        , minx=MinX, maxx=MaxX, yticks=YTicks, xticks=8
        , miny=MinY, maxy=MaxY, values=Data
        , miny2=MinY2, maxy2=MaxY2}
.

