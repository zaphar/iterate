-module(element_backlog_panel).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("macros.hrl").
-include("elements.hrl").
-include("iterate_records.hrl").

render(ControlId, Record) ->
    PanelId = "backlog_panel"
    , Data    = case Record#backlog_panel.data of
        undefined ->
            [];
        D when is_list(D) ->
            D
    end
    , Panel = #rounded_panel{ body=[#list{ id=PanelId, body=backlogs(Data) },
        #link{ text="create", 
           actions=#event{ 
               type=click, delegate=?MODULE, 
               postback=?B_PANEL_CREATE
           }}]
    }
    , element_rounded_panel:render(ControlId, Panel)
.

%% generate our backlog list
backlogs([]) ->
    [];
backlogs([H|T]) ->
    Name = H#backlogs.backlog_name
    , io:format("adding backlog: ~p~n", [H])
    , [ #listitem{ body=#backlog{backlog_name=Name} } | backlogs(T) ]
.

%% showing backlog info
event({show, {backlog, Name}}) ->
    wf:update(Name ++ "_target",
        #backlog_edit{backlog_id=Name, desc="A description"});
event({remove, {backlog, Name}}) ->
    wf:update(Name ++ "_target", "");
event(?B_PANEL_CREATE) ->
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
    case iterate_db:backlog({new, #backlogs{backlog_name=Value}}) of
        {error, Msg} ->
            wf:update(PanelId, "Failed!!"),
            wf:flash(io_lib:format("~p", [Msg]));
        {atomic, ok} ->
            wf:update(PanelId, io_lib:format("Backlog ~p Created", [Value])),
            wf:insert_top(?BPANELID, #backlog{backlog_name=Value}); 
        _ ->
            throw({error, unknown})
    end,
    ok;
event(Event) ->
    io:format("recieved event: ~p~n", [Event]),
    ok
.

