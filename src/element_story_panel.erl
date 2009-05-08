-module(element_story_panel).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("events.hrl").
-include("elements.hrl").
-include("iterate_records.hrl").

render() ->
    wf:render(#panel{id=story_box, body=#story_panel{data=undefined}})
.

render(ControlId, Record) ->
    PanelId = wf:temp_id()
    , Data = stories(Record#story_panel.data)
    , {Type, Name} = for_what()
    , Actions = [#link{ text="create"
          , actions=#event{
              type=click, delegate=?MODULE
              , postback=?S_PANEL_CREATE(Type, Name)
          }}, " "
       , #link{ text="refresh"
          , actions=#event{
              type=click, delegate=?MODULE
              , postback=?SHOW_STORIES(Type, Name)
          }}
    ]
    , Panel = #rounded_panel{ id=PanelId,
        body=[#span{text="Stories", class=panel_title}, #br{}, #br{} 
              , #panel{id=?SPANELID
                , class=story_panel
                , body=Data }] ++ Actions
    }
    , element_rounded_panel:render(ControlId, Panel)
.

stories(undefined) ->
    io:format("Default is stories for: ~p", [for_what()])
    , {Type, Name} = for_what()
    , L = case Type of
        iteration ->
            [ S#stories.story_name || S <- 
                iterate_wf:get_iteration_stories(Name) ];
        backlog ->
            [ S#stories.story_name || S <- 
                iterate_wf:get_backlog_stories(Name) ]
    end
    , stories(L, for_what());
stories(L) ->
    io:format("Asked for stories for: ~p", [for_what()])
    , stories(L, for_what()).

stories([], {_Type, _Name}) ->
    []; 
stories([H|T], {Type, Name}) ->
    [ #story{story_name=H} | stories(T, {Type, Name}) ]
.

for_what() ->
    iterate_wf:working_in()
.

for_what(type) ->
    {Type, _} = for_what()
    , Type;
for_what('name') ->
    {_, N} = for_what()
    , N
.

%% showing stories
event(?SHOW_STORIES(iteration, Name)) ->
    StoryList = [ S#stories.story_name || S <- 
        iterate_wf:get_iteration_stories(Name) ]
    , wf_session:session(working_in, {iteration, Name})
    , wf:update(story_box, #story_panel{data=StoryList} )
    , wf:update(report_panel, #report_panel{});
event(?SHOW_STORIES(backlog, Name)) ->
    StoryList = [ S#stories.story_name || S <- 
        iterate_wf:get_backlog_stories(Name) ]
    , wf_session:session(working_in, {backlog, Name})
    , wf:update(story_box, #story_panel{data=StoryList} )
    , wf:update(report_panel, #report_panel{});
event(?S_PANEL_CREATE(_Type, undefined)) ->
    wf:flash("can't create stories without a backlog or iteration");
event(?S_PANEL_CREATE(iteration, Backlog)) ->
    %% we need a create story widget
    TB_Id = wf:temp_id()
    , PanelId = wf:temp_id()
    , ButtonId = wf:temp_id()
    , wf:flash(#panel{ id=PanelId
        , body=[
            "creating story for iteration: " ++ Backlog, #br{ }
            , #textbox{ id=TB_Id, next=ButtonId,  text="Enter Name Here"}
            , #button{ id=ButtonId,
                text="Create",
                actions=#event{ delegate=?MODULE,
                    type=click, postback=?CREATE_S(TB_Id, PanelId
                        , {iteration, Backlog})}
        }]
    })
    , ok;
event(?S_PANEL_CREATE(backlog, Backlog)) ->
    %% we need a create story widget
    TB_Id = wf:temp_id()
    , PanelId = wf:temp_id()
    , ButtonId = wf:temp_id()
    , wf:flash(#panel{ id=PanelId
        , body=[
            "creating story for backlog: " ++ Backlog, #br{ }
            , #textbox{ id=TB_Id, next=ButtonId,  text="Enter Name Here"}
            , #button{ id=ButtonId,
                text="Create",
                actions=#event{ delegate=?MODULE,
                    type=click, postback=?CREATE_S(TB_Id, PanelId
                        , {backlog, Backlog})}
        }]
    })
    , ok;
%% TODO(jwall): refactor these two they are identical
event(?CREATE_S(Id, PanelId, {iteration, Backlog})) ->
    [Value] = wf:q(Id)
    , try 
        iterate_wf:create_story_for(Value, {iteration, Backlog})
        , wf:update(PanelId, io_lib:format("Story ~p Created", [Value]))
        , event(?SHOW_STORIES(iteration, Backlog))
    catch
        Msg ->
            wf:update(PanelId, "Failed!!")
            , wf:flash(io_lib:format("~p", [Msg]))

    end
    , ok;
event(?CREATE_S(Id, PanelId, {backlog, Backlog})) ->
    [Value] = wf:q(Id)
    , try 
        iterate_wf:create_story_for(Value, {backlog, Backlog})
        , wf:update(PanelId, io_lib:format("Story ~p Created", [Value]))
        , event(?SHOW_STORIES(backlog, Backlog))
    catch
        Msg ->
            wf:update(PanelId, "Failed!!")
            , wf:flash(io_lib:format("~p", [Msg]))

    end
    , ok;
event(Event) -> 
    io:format("recieved unknown event: ~p~n", [Event]),
    ok
.

