-module(element_iteration_panel).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("events.hrl").
-include("elements.hrl").
-include("iterate_records.hrl").

%% TODO(jwall): these panels are getting general enough I think a 
%5              refactor is in order.
render(_ControlId, Record) ->
    PanelId = wf:temp_id()
    , ButtonsId = wf:temp_id()
    , ContentId = wf:temp_id()
    , Data  = case Record#iteration_panel.data of
        undefined ->
            [];
        D when is_list(D) ->
            D
    end
    , Panel = #rounded_panel{ id=PanelId, body=[
        #panel{id=ButtonsId, body=["Iterations"]}
        , #panel{id=ContentId, body=iterations(Data)}]}
    , element_rounded_panel:render(PanelId, Panel)
.

%% generate our backlog list
iterations([]) ->
    [];
iterations([H|T]) ->
    Name = H#iterations.iteration_name
    , io:format("adding iteration: ~p~n", [H])
    , [ #iteration{iteration_name=Name} | iterations(T) ]
.

event(Event) ->
    io:format("~p recieved event: ~p~n", [?MODULE, Event]),
    ok
.

