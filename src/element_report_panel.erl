-module(element_report_panel).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("stats.hrl").
-include("elements.hrl").

-import(iterate_report, [completion_for_last_week/1]).
-define(GRAPHW, 600).
-define(GRAPHH, 150).


reflect() -> record_info(fields, report_panel).

render() ->
    wf:render(#panel{id=report_panel, body=#report_panel{}})
.

render(ControlId, _Record) ->
    PanelId = wf:temp_id()
    , io:format("Creating report for: ~p~n", [iterate_wf:working_in()])
    , CChart = build_completion_chart(iterate_wf:working_in())
    , TagChart = build_tag_chart(iterate_wf:working_in())
    , Panel = #panel{class="report_panel"
        , id=PanelId, body=["Completion/Story Point chart"
        , CChart, "Tag Spread Chart", TagChart]}
    , element_panel:render(ControlId, Panel)
.

build_tag_chart(undefined) ->
    "";
build_tag_chart({Type, Name}) ->
    {Total, DataSet} = iterate_report:tag_spread({Type, Name})
    , Width = ?GRAPHW
    , Height = ?GRAPHH
    , MinX = 0 
    , MaxX = length(DataSet)
    , Ticks = case DataSet of
        [] ->
            [];
        _ ->
            element_flot_chart:generate_ticks([ X || {X, _} <- DataSet ])
    end
    , MinY = 0
    , MaxY = Total
    , {_, Transformed} = lists:foldl(fun({_, Y}, {I, L}) ->
        {I+1, [{I+1, Y} | L]}
    end, {-1, []}, DataSet)
    , Data = [{"tag spread", [ [I, Y] || {I, Y} <- Transformed ]}]
    , #flot_chart{width=Width, height=Height, lines=false, points=false
        , bar=true, minx=MinX, maxx=MaxX, xticks=Ticks
        , miny=MinY, maxy=MaxY, values=Data}
.
build_completion_chart(undefined) ->
    "";
build_completion_chart({Type, Name}) ->
    TS1 = iterate_report:story_points_completed_for_last_week({Type, Name})
    , TS2 = iterate_report:story_points_incomplete_for_last_week({Type, Name})
    , TS3 = completion_for_last_week({Type, Name})
    % TODO(jwall): make this into a widget and blog about it
    , {Complete, Incomplete} = iterate_wf:iteration_story_points(Name)
    , Width = ?GRAPHW
    , Height = ?GRAPHH
    , MinX = date_util:date_to_epoch(date_util:subtract(date(), {days, 7})) * 1000 
    , MaxX = date_util:date_to_epoch(date_util:add(date(), {days, 2})) * 1000
    , YTicks = 5
    , MinY = 0
    , MaxY = 100
    , MinY2 = 0
    , MaxIncomplete = lists:max([R#tsentry.value || R <- TS2])
    , MaxY2 = case (Complete + Incomplete) > MaxIncomplete of
        true ->
            Complete + Incomplete;
        false ->
            MaxIncomplete
    end
    , Data = [{"Completion", [[V#tsentry.epoch, V#tsentry.value] || V <- TS3]}
        , {"SP burnup", [[V#tsentry.epoch, V#tsentry.value] || V <- TS1], {yaxis, 2}}
        , {"SP burndown", [[V#tsentry.epoch, V#tsentry.value] || V <- TS2], {yaxis, 2}}]
    , #flot_chart{width=Width, height=Height, modex="time"
        , minx=MinX, maxx=MaxX, yticks=YTicks, xticks=8
        , miny=MinY, maxy=MaxY, values=Data
        , miny2=MinY2, maxy2=MaxY2}
.

