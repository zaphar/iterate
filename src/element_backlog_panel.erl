-module(element_backlog_panel).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("events.hrl").
-include("elements.hrl").
-include("iterate_records.hrl").

-define(SEARCHBOX, search_backlogs_box).

render() ->
    wf:render(#panel{id="backlogs"
        , body=#backlog_panel{data=iterate_wf:get_backlogs()}}).

%% TODO(jwall): these panels are getting general enough I think a 
%%              refactor is in order.
render(ControlId, Record) ->
    PanelId = wf:temp_id()
    , Msg = wf:f("the control id is ~p~n", [ControlId])
    , iterate_log:log_debug(Msg)
    , Filter = Record#backlog_panel.filter
    , Data    = case Record#backlog_panel.data of
        undefined ->
            [];
        D when is_list(D) ->
            D
    end
    , SearchId = ?SEARCHBOX
    , SearchEvent = #event{type=change, delegate=?MODULE,
        postback={search, SearchId, ControlId}}
    , SearchFocusEvent = #event{type=focus, actions=["obj('me').select();"]}
    , Panel = #panel{ class="backlog_panel", body=[
        #span{text="Backlogs", class=panel_title}, #br{}, #br{}
        , #textbox{id=SearchId, text=Filter
            , style="margin-bottom: 6px;"
            , class=input_box
            , actions=[SearchEvent
                , SearchFocusEvent]}, #br{}
        , #panel{class="menu", id=PanelId, body=backlogs(Data, ControlId)}]}
    , element_panel:render(ControlId, Panel)
.

%% generate our backlog list
backlogs([], Id) ->
    [#link{ text="create", 
           actions=#event{ 
               type=click, delegate=?MODULE, 
               postback=?B_PANEL_CREATE(Id)
           }}, " ",
     #link{ text="view all", 
           actions=#event{ 
               type=click, delegate=?MODULE, 
               postback=?REFRESH(undefined)
           }}
    ];
backlogs([H|T], Id) ->
    Name = H#backlogs.backlog_name
    , Msg = wf:f("adding backlog: ~p~n", [H])
    , iterate_log:log_debug(Msg)
    , [ #backlog{backlog_name=Name, container=Id} | backlogs(T, Id) ]
.

%% TODO(jwall): abstract these callbacks
%5              out to a general panel callback module?
%% showing backlog info
event(?B_EDIT_SHOW(Name)) ->
    wf:update(Name ++ "_target",
        #backlog_edit{backlog_id=Name, desc="A description"});
event(?B_EDIT_REMOVE(Name)) ->
    wf:update(Name ++ "_target", "");
event(?B_PANEL_CREATE(_Id)) ->
    %% we need a create backlog widget
    TB_Id = wf:temp_id(),
    PanelId = wf:temp_id(),
    ButtonId = wf:temp_id(),
    element_notify:msg(#panel{ id=PanelId 
        , body=[#textbox{ id=TB_Id, next=ButtonId,  text="Enter Name Here"}
        , #button{ id=ButtonId,
            text="Create",
            actions=#event{ delegate=?MODULE, 
                type=click, postback=?CREATE_B(TB_Id, PanelId)}
        }]
    }),
    ok;
event(?CREATE_B(Id, PanelId)) ->
    [Value] = wf:q(Id),
    case iterate_wf:create_backlog(Value) of
        {error, Msg} ->
            wf:update(PanelId, "Failed!!"),
            element_notify:msg(io_lib:format("~p", [Msg]), 600);
        {atomic, ok} ->
            wf:update(PanelId, io_lib:format("Backlog ~p Created", [Value])),
            event(?REFRESH(undefined)); 
        _ ->
            throw({error, unknown})
    end,
    ok;
event(?REFRESH(_Id)) ->
    %% TODO(jwall): refresh should honor the search box
    wf:update(backlogs, #backlog_panel{data=iterate_wf:get_backlogs()});
event(?B_PANEL_SEARCH(Id, PanelId)) ->
    [Value] = wf:q(Id)
    , Msg = wf:f("~p searching for: ~p~n", [?MODULE, Value])
    , iterate_log:log_debug(Msg)
    , Results = iterate_wf:search_for_backlog(Value)
    , iterate_log:log_debug(wf:f("~p found: ~p~n", [?MODULE, Results]))
    % TODO(jwall): the filter box needs to keep the search terms
    , wf:update(PanelId, #backlog_panel{data=Results, filter=Value})
    %%, wf:wire(?SEARCHBOX, "obj('me').focus(); obj('me').select();")
    , ok;
event(Event) ->
    Msg = wf:f("~p recieved event: ~p~n", [?MODULE, Event])
    , iterate_log:log_debug(Msg)
    , ok
.

