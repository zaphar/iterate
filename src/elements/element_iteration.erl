-module(element_iteration).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("stats.hrl").
-include("elements.hrl").
-include("events.hrl").
-include("iterate_records.hrl").

render_element(Record) ->
    Name = Record#iteration.iteration_name
    , PanelId = wf:temp_id()
    , Body = body(Name, PanelId)
    %, iterate_log:log_info(wf:f("Body: ~p~n", [Body]))
    , Panel = #droppable{ id=PanelId
        , class="panel_element backlog_element iteration_element" ++ selection_class(Name)
        , hover_class=drop_hover
        , delegate=?MODULE
        , tag=Name
        , body=Body
    }
    , iterate_log:log_debug(wf:f("the panel id for ~s is ~s~n"
        , [Name, PanelId]))
    , Panel
.

is_working_in({iteration, Name}, Me) ->
    Me == Name;
is_working_in(_, _) ->
    false
.

selection_class(Me) ->
    case is_working_in(wf:session(working_in), Me) of
        true ->
            " selected";
        false ->
            ""
    end
.

is_whitespace(C) -> lists:any(fun (C2) -> C2 == C end, " \n\r\t").

body(Name, PanelId) ->
    NameStripped = lists:filter(fun (C) -> not is_whitespace(C) end, Name)
    , iterate_log:log_info(wf:f("NameStripped: ~p~n", [NameStripped]))
    , Jscript = "if ($(obj('me')).hasClass('selected')) {"
               ++ "$(obj('me')).addClass('selected');"
            ++ "} else {" 
               ++ "$('.backlog_element.selected').removeClass('selected');" 
               ++ "$(obj('me')).addClass('selected');"
            ++ "}"
    %%TODO(jwall): same for delete and edit
    , {CloseText, ClosePB} =
        case iteration_util:started(hd(iterate_db:iteration(?Q_ITERATION(Name)))) of
            false ->
                {"restart", {reopen, Name, PanelId}};
            true ->
                {"close", ?CLOSE_I_EL(Name, PanelId)}
        end
    , Title = wf:f("~s  ~.1f%"
        , [Name, iterate_wf:iteration_completion(Name)])
    %TODO(jwall): the click event needs to be handled by the page
    , [#panel{ id=PanelId, actions=#event{type=click
                %, delegate=element_story_panel
                %, postback=?SHOW_STORIES(iteration, Name)
                , postback={click, {iteration, Name}}
                , actions=Jscript
             }
             , body=[#panel{ class=bold
                             , id=NameStripped ++ "_name", body=Title
                           }
                 , " " , #link{text="edit"
                         , actions=#event{type=click, delegate=?MODULE
                             %, override=true
                             , postback=?SHOW_B_EL(Name, PanelId)
                         }
                 }
                 %TODO(jwall): delete and close events should delegate to their parent?
                 , " " , #link{text="delete"
                         , actions=#event{type=click, delegate=?MODULE
                             %, override=true
                             , postback=?DELETE_B_EL(Name, PanelId)
                         }
                 }
                 , " " , #link{text=CloseText
                         , actions=#event{type=click, delegate=?MODULE
                             %, override=true
                             , postback=ClosePB
                         }
                 }
             ]
    }
    , #panel{id=[NameStripped ++ "_target"]}]
.

%% showing iteration info
event(?UPDATE_B_EL(Name, Id)) ->
    case iterate_db:iteration(?Q_ITERATION(Name)) of
        [_ | []] ->
            wf:update(Id, body(Name, Id))
    end;
event(?SHOW_B_EL(Name, Id)) ->
    case iterate_db:iteration(?Q_ITERATION(Name)) of
        [B | []] ->
            wf:update(Name ++ "_target",
                #iteration_edit{ iteration_id=Name, el_id=Id, 
                    desc=B#iterations.desc })
    end;
event(?DELETE_B_EL(Name, Id)) ->
    iterate_wf:delete_iteration(Name)
    , event(?REMOVE_B_EL(Name, Id));
event(?CLOSE_I_EL(Name, Id)) ->
    iterate_wf:close_iteration(Name)
    , event(?REMOVE_B_EL(Name, Id));
event({reopen, Name, _}) ->
    iterate_wf:open_iteration(Name)
    , element_iteration_panel:event(?REFRESH(all));
event(?REMOVE_B_EL(Name, _)) ->
    iterate_wf:stop_working_in({iteration, Name}) 
    , element_iteration_panel:event(?REFRESH(undefined));
event(Event) -> 
    iterate_log:log_debug(wf:f("received event: ~p~n", [Event]))
.

%% move a story to a backlog
drop_event(Story, Iteration) ->
    iterate_log:log_debug(wf:f("received event: ~p -> ~p~n"
        , [Story, Iteration]))
    , [StoryRecord | []] = iterate_db:story(?Q_STORY(Story))
    , Backlog = StoryRecord#stories.backlog
    , iterate_stats:record(story
        , ?MOVE_STAT(Story, Iteration, Backlog))
    , iterate_log:log_debug(wf:f("found story: ~p ~n", [StoryRecord]))
    , NewStory = story_util:set_iteration(StoryRecord, Iteration)
    , iterate_log:log_debug(wf:f("changed story to: ~p ~n", [NewStory]))
    , iterate_db:story(?Q_UPDATE_STORY(NewStory))
    , element_notify:msg(
        io_lib:format("Took on Story: ~p in Iteration: ~p", [Story, Iteration])
            , 600)
    , {Type, _Name} = story_util:get_type(StoryRecord)
    , element_story_panel:event(?SHOW_STORIES(Type, Backlog))
    , element_iteration_panel:event(?REFRESH(bogus_id))
.

get_iter(Name) ->
    case iterate_db:iteration(?Q_ITERATION(Name)) of
        [Iteration] ->
            {ok, Iteration};
        [] ->
            {abort, no_such_iteration}
    end
.

