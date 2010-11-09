-module(element_backlog_panel).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("events.hrl").
-include("elements.hrl").
-include("iterate_records.hrl").

-define(SEARCHBOX, search_backlogs_box).

render() ->
    #panel{id="backlogs"
      , body=#backlog_panel{data=iterate_wf:get_backlogs()}}
.

%% TODO(jwall): these panels are getting general enough I think a 
%%              refactor is in order.
render_element(Record) ->
    PanelId = wf:temp_id()
    , Filter = Record#backlog_panel.filter
    , Data    = case Record#backlog_panel.data of
        undefined ->
            [];
        D when is_list(D) ->
            D
    end
    , SearchId = ?SEARCHBOX
    , SearchEvent = #event{type=change, delegate=?MODULE,
        postback=search}
    , SearchFocusEvent = #event{type=focus, actions=["obj('me').select();"]}
    , Panel = #panel{ class="backlog_panel", body=[
        #span{text="Backlogs", class=panel_title}, #br{}, #br{}
        , "Filter Backlogs:", #br{}
        , #textbox{id=SearchId, text=Filter
            , style="margin-bottom: 6px;"
            , class=input_box
            , actions=[SearchEvent
                , SearchFocusEvent]}, #br{}
	%% at some point we no longer want to hard code this id :-).
        , #panel{class="menu", id=PanelId, body=backlogs(Data, "backlogs")}]}
    , element_panel:render_element(Panel)
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
    TB_Id = wf:temp_id()
    , PanelId = wf:temp_id()
    , ButtonId = wf:temp_id()
    , TextBox = iterate_element_utils:autofocus_text_box(TB_Id
        , "Enter Name Here")
    , element_notify:msg(#panel{ id=PanelId 
        , body=[TextBox#textbox{next=ButtonId}
                , #button{ id=ButtonId
                    , text="Create"
                    , actions=#event{ delegate=?MODULE
                        , type=click, postback=?CREATE_B(TB_Id, PanelId)}
        }]
    }, {close, ButtonId}),
    ok;
event(?CREATE_B(Id, PanelId)) ->
    [Value] = wf:q(Id),
    case iterate_wf:create_backlog(Value) of
        {error, Msg} ->
            wf:update(PanelId, "Failed!!"),
            element_notify:msg(io_lib:format("~p", [Msg]), 600);
        {atomic, ok} ->
            wf:update(PanelId, io_lib:format("Backlog ~p Created", [Value])),
            refresh(); 
        _ ->
            throw({error, unknown})
    end,
    ok;
event(search) ->
    event(?REFRESH(undefined));
event(?REFRESH(undefined)) ->
    refresh();
event(?REFRESH(_Id)) ->
    refresh();
event(?B_PANEL_SEARCH(_Id, _PanelId)) ->
    refresh();
event(Event) ->
    Msg = wf:f("~p recieved event: ~p~n", [?MODULE, Event])
    , iterate_log:log_debug(Msg)
    , ok
.

refresh() ->
    case wf:q(?SEARCHBOX) of
        undefined  ->
            wf:update(backlogs, #backlog_panel{data=iterate_wf:get_backlogs()});
        ["Filter Backlogs"] ->
            wf:update(backlogs, #backlog_panel{data=iterate_wf:get_backlogs()});
        [] ->
            wf:update(backlogs, #backlog_panel{data=iterate_wf:get_backlogs()});
        [Value] ->
            Msg = wf:f("~p searching for: ~p~n", [?MODULE, Value])
            , iterate_log:log_debug(Msg)
            , Results = iterate_wf:search_for_backlog(Value)
            , iterate_log:log_debug(wf:f("~p found: ~p~n", [?MODULE, Results]))
            , wf:update(backlogs, #backlog_panel{data=Results, filter=Value})
    end
    , ok
.

update_backlog_panel() ->
    refresh()
.

