-module(element_story).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("macros.hrl").
-include("elements.hrl").

render(_ControlId, Record) ->
    %% TODO(jwall): change to temp ids wf:temp_id()
    PanelId = wf:temp_id()
    , DraggableId = wf:temp_id()
    , Name    = Record#story.story_name
    , [Story] = iterate_db:story(?Q_STORY(Name))
    , Percent = story_util:completion(Story)
    , Panel = #draggable{ id=DraggableId
                    , style="border-bottom: solid black 3px; padding: 3px;"
                    , tag=Name
                    , body=#panel{ id=PanelId
                        , body=[
                            Name
                            , " "
                            , #link{text="edit"
                                , actions=#event{ type=click, delegate=?MODULE
                                                , postback=?SHOW_S_EL(Name)
                                }
                            }, " "
                            , #link{text="delete"
                                , actions=#event{ 
                                    type=click
                                    , delegate=?MODULE
                                    , postback=?DELETE_S_EL(Name, PanelId)
                                }
                            }, #br{}
                            , #my_inplace_textbox{delegate=?MODULE,
                                text=io_lib:format("~.10B%", [Percent]),
                                tag={complete, {story, Name}} }
                            , #panel{id=Name ++ "_target"}
                        ]
    }}
    , element_draggable:render(DraggableId, Panel).

event(?SHOW_S_EL(Name)) ->
    wf:update(Name ++ "_target",
        #story_edit{story_name=Name, desc="A description", sp="3"});
event(?REMOVE_S_EL(Name)) ->
    wf:update(Name ++ "_target", "");
event(?DELETE_S_EL(Name, Id)) ->
    Records = iterate_db:story(?Q_STORY(Name))
    , [Record | []] = Records
    , _Result = iterate_db:story({delete, Record})
    , wf:wire(Id, #hide{ effect=slide, speed=500 })
    , event(?REMOVE_S_EL(Name))
    , ok;
event(_) -> 
    ok
.

inplace_textbox_event({complete, {story, Name}}, Value) ->
    Percent = list_to_integer(
        case lists:last(Value) of
            $% ->
                lists:sublist(Value, length(Value) - 1);
            _  ->
                Value
        end)
    , [Original] = iterate_db:story(?Q_STORY(Name))
    , New = story_util:set_percent(Original, Percent)
    , iterate_db:story({update, New})
    , ReturnValue = lists:flatten(io_lib:format("~B%", [(Percent)]))
    , io:format("~p~n", [ReturnValue])
    , ReturnValue
.

