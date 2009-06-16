-module(element_iteration_panel).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("events.hrl").
-include("elements.hrl").
-include("iterate_records.hrl").

%% TODO(jwall): these panels are getting general enough I think a 
%%              refactor is in order.

render() -> render(started).

render(Type) when Type == started orelse Type == closed orelse Type == all ->
    wf:render(#panel{id=iteration_panel
        , body=#iteration_panel{data=iterate_db:iterations(Type), type=Type}})
.

render(ControlId, Record) ->
    %% TODO(jwall): add a filter like in backlogs
    PanelId = wf:temp_id()
    , iterate_log:log_debug(wf:f("creating an interation panel for ~p iterations~n",
        [Record#iteration_panel.type]))
    , ButtonsId = wf:temp_id()
    , ContentId = wf:temp_id()
    , Data  = case Record#iteration_panel.data of
        undefined ->
            [];
        D when is_list(D) ->
            D
    end
    , Panel = #panel{ class="iteration_panel", id=PanelId, body=[
        #panel{id=ButtonsId, body=[
            #hidden{id=iteration_panel_type, text=Record#iteration_panel.type}
            , #span{text="Iterations", class=panel_title}, #br{}, #br{}
            , #link{text="Start Iteration"
                    , actions=#event{
                        type=click
                        , delegate=?MODULE
                        , postback=?STARTITER(ContentId)}}]}
        , #panel{class="menu", id=ContentId, body=iterations(Data)}]}
    , element_panel:render(ControlId, Panel)
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

event(?REFRESH(all)) ->
    wf:update(iteration_panel
        , #iteration_panel{data=iterate_wf:get_all_iterations(), type=all});
event(?REFRESH(closed)) ->
    wf:update(iteration_panel
        , #iteration_panel{data=iterate_wf:get_closed_iterations()
            , type=closed});
event(?REFRESH(started)) ->
    wf:update(iteration_panel
        , #iteration_panel{data=iterate_wf:get_started_iterations()
            , type=started});
event(?REFRESH(_)) ->
    case wf:q(iteration_panel_type) of
        [] ->
            %% error condition
            iterate_log:log_debug("trying to render an iteration panel that doesn't know what type of panel he is");
        undefined ->
            %% error condition
            iterate_log:log_debug("trying to render an iteration panel that doesn't know what type of panel he is");
        [Type] ->
            iterate_log:log_info(wf:f("refreshing iteration panel for ~p iterations", [list_to_atom(Type)]))
            , event(?REFRESH(list_to_atom(Type)))
    end;
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
    iterate_log:log_debug(wf:f("~p recieved event: ~p~n", [?MODULE, Event])),
    ok
.

update_iteration_panel() ->
    wf:update(iteration_panel
        , #iteration_panel{data=iterate_wf:get_started_iterations()})
.
