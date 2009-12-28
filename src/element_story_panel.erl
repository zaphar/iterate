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
    , Panel = #panel{ class="story_panel", id=PanelId,
        body=[#span{text="Stories", class=panel_title}, #br{}, #br{} 
              , #panel{class="story_list", id=?SPANELID
                , body=Data }] ++ Actions
    }
    , element_panel:render(ControlId, Panel)
.

stories(undefined) ->
    iterate_log:log_debug(wf:f("Default is stories for: ~p", [for_what()]))
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
    iterate_log:log_debug(wf:f("Asked for stories for: ~p", [for_what()]))
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
event(?REFRESH(_Id)) ->
    {Type, Name} = iterate_wf:working_in()
    , iterate_log:log_debug(wf:f("refreshing stories for: ~p", [{Type, Name}]))
    , event(?SHOW_STORIES(Type, Name));
event(?SHOW_STORIES(iteration, Name)) ->
    StoryList = [ S#stories.story_name || S <- 
        iterate_wf:get_iteration_stories(Name) ]
    , iterate_wf:working_in({iteration, Name})
    , wf:update(story_box, #story_panel{data=StoryList} )
    , wf:update(report_panel, #report_panel{});
event(?SHOW_STORIES(backlog, Name)) ->
    StoryList = [ S#stories.story_name || S <- 
        iterate_wf:get_backlog_stories(Name) ]
    , iterate_wf:working_in({backlog, Name})
    , wf:update(story_box, #story_panel{data=StoryList} )
    , wf:update(report_panel, #report_panel{});
event(?S_PANEL_CREATE(_Type, undefined)) ->
    element_notify:msg("can't create stories without a backlog or iteration"
        , 600);
event(?S_PANEL_CREATE(iteration, Backlog)) ->
    %% we need a create story widget
    TB_Id = wf:temp_id()
    , PanelId = wf:temp_id()
    , ButtonId = wf:temp_id()
    , TextBox = iterate_element_utils:autofocus_text_box(TB_Id, "Enter Name Here")
    , element_notify:msg(#panel{ id=PanelId
        , body=[
            "creating story for iteration: " ++ Backlog, #br{ }
            , TextBox#textbox{next=ButtonId}
            , #button{ id=ButtonId,
                text="Create",
                actions=#event{ delegate=?MODULE,
                    type=click, postback=?CREATE_S(TB_Id, PanelId
                        , {iteration, Backlog})}
        }]
    }, {close, ButtonId})
    , ok;
event(?S_PANEL_CREATE(backlog, Backlog)) ->
    %% we need a create story widget
    TB_Id = wf:temp_id()
    , PanelId = wf:temp_id()
    , ButtonId = wf:temp_id()
    , element_notify:msg(#panel{ id=PanelId
        , body=[
            "creating story for backlog: " ++ Backlog, #br{ }
            , #textbox{ id=TB_Id, next=ButtonId,  text="Enter Name Here"}
            , #button{ id=ButtonId,
                text="Create",
                actions=#event{ delegate=?MODULE,
                    type=click, postback=?CREATE_S(TB_Id, PanelId
                        , {backlog, Backlog})}
        }]
    }, {close, ButtonId})
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
            , element_notify:msg(io_lib:format("~p", [Msg]), 600)

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
            , element_notify:msg(io_lib:format("~p", [Msg]), 600)

    end
    , ok;
event(Event) -> 
    iterate_log:log_debug(wf:f("recieved unknown event: ~p~n", [Event]))
    , ok
.

update_story_list() ->
    StoryList = case iterate_wf:working_in() of
        {iteration, Name} ->
            [ S#stories.story_name || S <- 
                iterate_wf:get_iteration_stories(Name) ];
        {backlog, Name} ->
            [ S#stories.story_name || S <- 
                iterate_wf:get_backlog_stories(Name) ]
    end
    , wf:update(story_box, #story_panel{data=StoryList} )
    , wf:update(report_panel, #report_panel{})
.
