-module(element_flot_chart).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("elements.hrl").

reflect() -> record_info(fields, flot_chart).

render(ControlId, Record) ->
    %% handle multiple datasets
    Data = Record#flot_chart.values
    , DataSet = data_as_js(Data)
    %% description of the graph
    , _Title = Record#flot_chart.title
    , Width = Record#flot_chart.width
    , Height = Record#flot_chart.height
    , MinX = Record#flot_chart.minx 
    , MaxX = Record#flot_chart.maxx
    , MinY = Record#flot_chart.miny
    , MaxY = Record#flot_chart.maxy
    , _XTicks = Record#flot_chart.yticks
    , YTicks = Record#flot_chart.yticks
    , Lines = Record#flot_chart.lines
    , Points = Record#flot_chart.points
    , SelectMode = Record#flot_chart.selectmode
    %% IDs for the elements
    , TargetId = wf:temp_id()
    , GraphId = wf:temp_id()
    , ScriptId = wf:temp_id()
    , PlotId = case Record#flot_chart.id of
        undefined ->
            wf:temp_id();
        Id ->
            Id
    end
    , ToolTipId = wf:temp_id()
    %% TODO(jwall): colors
    , Script = wf:f("<script id=~p language='javascript', type='text/javascript'>"
        ++ "var " ++ PlotId ++ ";"
        ++ "$(function() { ~n"
        ++ "var d = ~s;~n" % DataSet
        ++ PlotId ++ " = $.plot($('#' + ~p), d, {xaxis: { mode: 'time',~n" % TargetId
        ++ " ticks: 7,~n"
        ++ " min: (new Date(~p).getTime()),~n" % MinX
        ++ " max: (new Date(~p).getTime())~n"  % MaxX
        ++ " }, ~n"
        ++ "yaxis: {ticks: ~p,~n" % YTicks
        ++ "min: ~p,~n" % MinY
        ++ "max: ~p,~n" % MaxY
        ++ "},~n"
        % TODO(jwall): lines and points should be modifiable
        ++ "lines: {show: ~p},~n" % Lines
        ++ "points: {show: ~p},~n" % Points
        % TODO(jwall): selection mode should be modifiable
        ++ "selection: {mode: ~p},~n" % SelectMode
        ++ "grid: {hoverable: true, clickable: true},~n"
        ++ "});~n"
        %% tooltip code
        ++ "function showTooltip(x, y, contents) {~n"
        ++ " $('<div id=~p>' + contents + '</div>').css( {~n" % ToolTipId
        ++ "     position: 'absolute',~n"
        ++ "     display: 'none',~n"
        ++ "     top: y + 5,~n"
        ++ "     left: x + 5,~n"
        ++ "     border: '1px solid #fdd',~n"
        ++ "     padding: '2px',~n"
        ++ "     'background-color': '#fee',~n"
        ++ "     opacity: 0.80~n"
        ++ " }).appendTo('body').fadeIn(200);~n"
        ++ "}~n"
        % now bind to the plothover event
        ++ "var previousPoint = null;~n"
        ++ "$('#' + ~p).bind('plothover', function(event, pos, item) {~n" % TargetId
        ++ " $('#x').text(pos.x.toFixed(2));~n"
        ++ " $('#y').text(pos.y.toFixed(2));~n"
        ++ " if (item) {~n"
        ++ "  if(previousPoint != item.datapoint) {~n"
        ++ "   previousPoint = item.datapoint;~n"
        ++ "   $('#' + ~p).remove();~n" % ToolTipId
        ++ "   var x = item.datapoint[0].toFixed(2),~n"
        ++ "       y = item.datapoint[1].toFixed(2);~n"
        ++ "   showTooltip(item.pageX, item.pageY, y + '%');~n"
        ++ "  } else {~n"
        ++ "   $('#' + ~p).remove();~n" % ToolTipId
        ++ "   previousPoint = null;~n"
        ++ "  }~n"
        ++ " }~n"
        ++ "})~n"
        %% TODO(jwall): select event custom?
        ++ "$('#' + ~p).bind('plotselected', function (event, ranges) {~n" % TargetId
        ++ " //alert(ranges.xaxis.from.toFixed(1) + ',' + ranges.yaxis.from.toFixed(1))~n"
        ++ " //alert(ranges.xaxis.to.toFixed(1) + ',' + ranges.yaxis.to.toFixed(1))~n"
        ++ "})~n"
        %% TODO(jwall): click event custom?
        ++ "$('#' + ~p).bind('plotclick', function (event, pos, item) {~n" % TargetId
        ++ " //alert(item.dataIndex + ' = ' + d[item.dataIndex]);~n"
        ++ "})~n"
        ++ "});~n</script>"
        , [ScriptId, DataSet, TargetId
            , MinX, MaxX, YTicks, MinY, MaxY
            , Lines, Points, SelectMode
            , ToolTipId, TargetId, ToolTipId, ToolTipId
            , TargetId, TargetId
        ])
    %% TODO(jwall): dataset manipulation
    %% TODO(jwall): zoom controls?
    %% TODO(jwall): pan buttons?
    , Panel = #panel{id=TargetId, style=wf:f("width:~ppx;height:~ppx", [Width, Height])}
    , element_panel:render(ControlId, #panel{id=GraphId, body=[Panel, Script]})
.

data_as_js(T) when is_tuple(T) ->
    data_as_js([T]);
data_as_js(L) ->
    List = data_as_js_preparse(L)
    , io:format("~p", [List])
    , "[" ++ string:join(List, ",") ++ "]"
.

data_as_js_preparse([]) ->
    [];
data_as_js_preparse([H | T]) when is_tuple(H) ->
   [ data_as_js_preparse(H) | data_as_js_preparse(T)];
data_as_js_preparse([H | T]) when is_list(H) ->
   [ data_as_js_preparse({"undefined", H}) | data_as_js_preparse(T)];
data_as_js_preparse({Label, Data}) when is_list(Data) ->
    wf:f("{ label: '~s', data: ~w}", [Label, Data])
.

