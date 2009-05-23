-module(element_iteration_panel).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("events.hrl").
-include("elements.hrl").
-include("iterate_records.hrl").

%% TODO(jwall): these panels are getting general enough I think a 
%%              refactor is in order.

render() ->
    wf:render(#panel{id=iteration_panel
        , body=#iteration_panel{data=iterate_db:iterations(started)}})
.

render(ControlId, Record) ->
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
        #panel{id=ButtonsId, body=[
            #span{text="Iterations", class=panel_title}, #br{}, #br{}
            , #link{text="Start Iteration"
                    , actions=#event{
                        type=click
                        , delegate=?MODULE
                        , postback=?STARTITER(ContentId)}}]}
        , #panel{id=ContentId, body=iterations(Data)}]}
    , element_rounded_panel:render(ControlId, Panel)
.

%% generate our backlog list
iterations() ->
    iterations(iterate_wf:get_started_iterations())
.

iterations([]) ->
    [];
iterations([H|T]) ->
    Name = H#iterations.iteration_name
    , [ #iteration{iteration_name=Name} | iterations(T) ]
.

event(?REFRESH(_Id)) ->
    wf:update(iteration_panel
        , #iteration_panel{data=iterate_wf:get_started_iterations()});
event(?STARTITER(IterPanelId)) ->
    PanelId = wf:temp_id()
    , TextBoxId = wf:temp_id()
    , ButtonId = wf:temp_id()
    , Panel = #panel{ id=PanelId, body=[ "Enter an Iteration Name: "
        , #textbox{ id=TextBoxId
            , next=ButtonId
            , actions=#event{ type=change
                , delegate=?MODULE
                , postback=?STARTITERTNAME(TextBoxId, PanelId, IterPanelId)}}
        , #button{id=ButtonId, text="Ok"}] }
    , element_notify:msg(Panel);
event(?STARTITERTNAME(TextBoxId, PanelId, IterPanelId)) ->
    [Value] = wf:q(TextBoxId)
    , iterate_wf:create_iteration(Value)
    , wf:update(PanelId, "created iteration: " ++ Value)
    , event(?REFRESH(IterPanelId));
event(Event) ->
    io:format("~p recieved event: ~p~n", [?MODULE, Event]),
    ok
.

