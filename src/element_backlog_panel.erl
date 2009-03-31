-module(element_backlog_panel).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("events.hrl").
-include("elements.hrl").
-include("iterate_records.hrl").

%% TODO(jwall): these panels are getting general enough I think a 
%5              refactor is in order.
render(ControlId, Record) ->
    PanelId = "backlog_panel"
    , io:format("the control id is ~p~n", [ControlId])
    , Data    = case Record#backlog_panel.data of
        undefined ->
            [];
        D when is_list(D) ->
            D
    end
    , SearchId = wf:temp_id()
    , SearchEvent = #event{type=change, delegate=?MODULE,
        postback={search, SearchId, ControlId}}
    , Panel = #rounded_panel{ body=[
        % TODO(jwall): the filter box should auto highlight 
        %              the text when clicked
        #textbox{id=SearchId, text="Filter Backlogs", actions=SearchEvent},
        #panel{id=PanelId, body=backlogs(Data, ControlId)}]}
    , element_rounded_panel:render(ControlId, Panel)
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
               postback=?REFRESH(Id)
           }}
    ];
backlogs([H|T], Id) ->
    Name = H#backlogs.backlog_name
    , io:format("adding backlog: ~p~n", [H])
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
    wf:flash(#panel{ id=PanelId 
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
            wf:flash(io_lib:format("~p", [Msg]));
        {atomic, ok} ->
            wf:update(PanelId, io_lib:format("Backlog ~p Created", [Value])),
            wf:insert_top("backlog_panel", #backlog{backlog_name=Value}); 
        _ ->
            throw({error, unknown})
    end,
    ok;
event(?REFRESH(Id)) ->
    wf:update(Id, #backlog_panel{data=iterate_db:backlogs()});
event(?B_PANEL_SEARCH(Id, PanelId)) ->
    [Value] = wf:q(Id),
    io:format("~p searching for: ~p~n", [?MODULE, Value]),
    Results = iterate_wf:search_for_backlog(Value),
    io:format("~p found: ~p~n", [?MODULE, Results]),
    % TODO(jwall): the filter box needs to keep the search terms
    wf:update(PanelId, #backlog_panel{data=Results}),
    ok;
event(Event) ->
    io:format("~p recieved event: ~p~n", [?MODULE, Event]),
    ok
.

