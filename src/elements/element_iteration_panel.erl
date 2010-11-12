-module(element_iteration_panel).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("events.hrl").
-include("elements.hrl").
-include("iterate_records.hrl").

-define(SEARCHBOX, search_iterations_box).

%% TODO(jwall): these panels are getting general enough I think a 
%%              refactor is in order.

render() -> render(started).

render(Type) when Type == started orelse Type == closed orelse Type == all ->
    #panel{id=iteration_panel
      , body=#iteration_panel{data=iterate_db:iterations(Type), type=Type}}
.

render_element(Record) ->
    %% TODO(jwall): add a filter like in backlogs
    PanelId = wf:temp_id()
    , iterate_log:log_debug(wf:f("creating an interation panel for ~p iterations~n",
        [Record#iteration_panel.type]))
    , Filter = Record#iteration_panel.filter
    , ButtonsId = wf:temp_id()
    , ContentId = wf:temp_id()
    , Data  = case Record#iteration_panel.data of
        undefined ->
            [];
        D when is_list(D) ->
            D
    end
    , SearchId = ?SEARCHBOX
    , SearchEvent = #event{type=change, delegate=?MODULE,
        postback=search}
    , SearchFocusEvent = #event{type=focus, actions=["obj('me').select();"]}
    , #panel{ class="iteration_panel", id=PanelId, body=[
        #panel{id=ButtonsId, body=[
            #hidden{id=iteration_panel_type, text=Record#iteration_panel.type}
            , #span{text="Iterations", class=panel_title}, #br{}, #br{}
            , #link{text="Start Iteration"
                    , actions=#event{
                        type=click
                        , delegate=?MODULE
                        , postback=?STARTITER(ContentId)}}]}
        , "Filter Iterations:", #br{}
        , #textbox{id=SearchId, text=Filter
            , style="margin-bottom: 6px;"
            , class=input_box
            , actions=[SearchEvent
                , SearchFocusEvent]}
        , #link{text="reset", actions=#event{
            type=click
            , delegate=?MODULE
            , actions=[wf:f("obj('~s').value = '';", [SearchId])]
            , postback=?REFRESH(Record#iteration_panel.type)}}, #br{}
        , #panel{class="menu", id=ContentId, body=iterations(Data)}]}
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

event(search) ->
    event(?REFRESH(undefined));
event(?REFRESH(all)) ->
    update_iteration_panel({all, iterate_wf:get_all_iterations()});
event(?REFRESH(closed)) ->
    update_iteration_panel({closed, iterate_wf:get_closed_iterations()});
event(?REFRESH(started)) ->
    update_iteration_panel({started, iterate_wf:get_started_iterations()});
event(?REFRESH(_)) ->
    %% searchbox trumps panel type
    case wf:q(?SEARCHBOX) of
        undefined  ->
            update_iteration_panel();
        []  ->
            update_iteration_panel();
        Value ->
            Msg = wf:f("~p searching for: ~p~n", [?MODULE, Value])
            , iterate_log:log_debug(Msg)
            , Results = iterate_wf:search_for_iteration(Value)
            , iterate_log:log_debug(wf:f("~p found: ~p~n", [?MODULE, Results]))
            , update_iteration_panel_data(Results)
    end;
event(?STARTITER(IterPanelId)) ->
    iterate_log:log_info("got startiter event")
    , PanelId = wf:temp_id()
    , TextBoxId = wf:temp_id()
    , ButtonId = wf:temp_id()
    , TextBox = iterate_element_utils:autofocus_text_box(
            TextBoxId, "Enter an Iteration Name: "
            , #event{ type=change
                , delegate=?MODULE
                , postback=?STARTITERTNAME(TextBoxId, PanelId, IterPanelId)})
    , Panel = #panel{ id=PanelId, body=[ "Enter an Iteration Name: "
        , TextBox#textbox{next=ButtonId}
        , #button{id=ButtonId, text="Ok"}] }
    , element_notify:msg(Panel, {close, ButtonId});
event(?STARTITERTNAME(TextBoxId, PanelId, IterPanelId)) ->
    Value = wf:q(TextBoxId)
    , iterate_wf:create_iteration(Value)
    , wf:update(PanelId, "created iteration: " ++ Value)
    %% TODO(jwall): wire an expire event to that panel.
    , event(?REFRESH(IterPanelId));
event(Event) ->
    iterate_log:log_debug(wf:f("~p recieved event: ~p~n", [?MODULE, Event])),
    ok
.

update_iteration_panel_data(Data) ->
    update_iteration_panel({get_panel_type(), Data})
.

update_iteration_panel({Type, Data}) ->
    Filter = get_search()
    , update_iteration_panel({Type, Data, Filter});
update_iteration_panel({Type, Data, Filter}) ->
    wf:update(iteration_panel, #iteration_panel{data=Data
                                                , type=Type
                                                , filter=Filter})
.

update_iteration_panel() ->
    Type = get_panel_type()
    , event(?REFRESH(Type))
.

get_search() ->
    case wf:q(?SEARCHBOX) of
        undefined  ->
            "";
        []         ->
            "";
        Value    ->
            Value
    end
.

get_panel_type() ->
    case wf:q(iteration_panel_type) of
        [] ->
            %% error condition
            iterate_log:log_debug("trying to render an iteration panel that doesn't know what type of panel he is")
            , {error, undefined_panel_type};
        undefined ->
            %% error condition
            iterate_log:log_debug("trying to render an iteration panel that doesn't know what type of panel he is")
            , {error, undefined_panel_type};
        Type ->
            iterate_log:log_info(wf:f("got an item back ~p", [Type]))
            , case is_atom(Type) of
              true -> Type;
              false -> list_to_atom(Type)
            end
    end
.

