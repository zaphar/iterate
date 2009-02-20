-module(element_story_panel).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("macros.hrl").
-include("elements.hrl").
-include("iterate_records.hrl").

render(ControlId, Record) ->
    %% TODO(jwall): change to temp ids wf:temp_id()
    PanelId = wf:temp_id()
    , Data    = case Record#story_panel.data of
        undefined ->
            [];
        D when is_list(D) ->
            D
    end
    , Panel = #rounded_panel{ id=PanelId,
        body=[#label{text="Stories"}, 
              #panel{id=?SPANELID, body=stories(Data) }]
    }
    , element_rounded_panel:render(ControlId, Panel)
.

stories(L) ->
    stories(L, undefined).

stories([], Name) ->
    [#link{ text="create",
        actions=#event{
            type=click, delegate=?MODULE,
            postback=?S_PANEL_CREATE(Name)
        }}, " ",
     #link{ text="refresh",
        actions=#event{
            type=click, delegate=?MODULE,
            postback=?SHOW_STORIES(Name)
        }}
    ]; 
stories([H|T], Name) ->
    [ #story{story_name=H, backlog=Name} | stories(T, Name) ]
.

%% showing stories
event(?SHOW_STORIES(Name)) ->
    StoryList = [ S#stories.story_name || S <- iterate_db:stories(Name) ],
    wf:update(story_list, stories(StoryList, Name) );
event(?S_PANEL_CREATE(undefined)) ->
    wf:flash("can't create stories without a backlog");
event(?S_PANEL_CREATE(Backlog)) ->
    %% we need a create backlog widget
    TB_Id = wf:temp_id(),
    PanelId = wf:temp_id(),
    ButtonId = wf:temp_id(),
    wf:flash(#panel{ id=PanelId
        , body=[
            "creating story for backlog: " ++ Backlog, #br{ }
            , #textbox{ id=TB_Id, next=ButtonId,  text="Enter Name Here"}
            , #button{ id=ButtonId,
                text="Create",
                actions=#event{ delegate=?MODULE,
                    type=click, postback=?CREATE_S(TB_Id, PanelId, Backlog)}
        }]
    }),
    ok;
event(?CREATE_S(Id, PanelId, Backlog)) ->
    [Value] = wf:q(Id),
    case iterate_db:story({new, #stories{story_name=Value, backlog=Backlog}}) of
        {error, Msg} ->
            wf:update(PanelId, "Failed!!"),
            wf:flash(io_lib:format("~p", [Msg]));
        {atomic, ok} ->
            wf:update(PanelId, io_lib:format("Story ~p Created", [Value])),
            wf:insert_top(?SPANELID, #story{story_name=Value});
        _ ->
            throw({error, unknown})
    end,
    ok;
event(Event) -> 
    io:format("recieved event: ~p~n", [Event]),
    ok
.

