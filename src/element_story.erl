-module(element_story).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("events.hrl").
-include("elements.hrl").
-include("iterate_records.hrl").

render(_ControlId, Record) ->
    PanelId = wf:temp_id()
    , PanelId2 = wf:temp_id()
    , DraggableId = wf:temp_id()
    , OrderElId = wf:temp_id()
    , Name    = Record#story.story_name
    , [Story] = iterate_db:story(?Q_STORY(Name))
    , Order = story_util:order(Story)
    , Percent = story_util:completion(Story)
    , Panel = #panel{ id=PanelId2
                    , class=story_element
                    , body=#panel{ id=PanelId
                        , body=[
                            #draggable{ tag=Name
                                , id=DraggableId
                                , body="[drag me] " }
                            , #textbox{ id=OrderElId
                                , actions=#event{
                                    type=change
                                    , delegate=?MODULE
                                    , postback=?ORDEDIT(Name, OrderElId)
                                }
                                , style="width: 20px;"
                                , text=lists:flatten(
                                    io_lib:format("~.10B", [Order]))}
                            , " - ", Name
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
                                tag=?COMPLETE_S(Name) }
                            , #panel{id=Name ++ "_target"}
                        ]
    }}
    , element_panel:render(PanelId2, Panel).

event(?SHOW_S_EL(Name)) ->
    wf:update(Name ++ "_target",
        #story_edit{story_name=Name, desc="A description", sp="3"});
event(?REMOVE_S_EL(Name)) ->
    wf:update(Name ++ "_target", "");
event(?DELETE_S_EL(Name, Id)) ->
    Records = iterate_db:story(?Q_STORY(Name))
    , [Record | []] = Records
    , _Result = iterate_db:story({delete, Record})
    , {Type, TypeName} = story_util:get_type(Record)
    , Actions = #event{type='timer', delay=1
        , actions=[
            #hide{ effect=slide, speed=500}
            , #event{ type='timer', delay=501
                , delegate=element_story_panel
                , postback=?SHOW_STORIES(Type, TypeName)}
        ]
    }
    , wf:wire(Id, Actions)
    , event(?REMOVE_S_EL(Name))
    , ok;
event(?ORDEDIT(Name, Id)) ->
    Value = case wf:q(Id) of
        [] ->
            0;
        [Found] ->
            Found
    end
    , io:format("updating order of ~p to ~p", [Name, Value])
    , Story = get_story(Name)
    , Updated = story_util:set_order(Story, Value)
    , iterate_db:story({update, Updated})
    , io:format("changing order ~p to ~p", [story_util:order(Story),
        story_util:order(Updated)])
    , Value;
event(Event) -> 
    io:format("~p received ~p event~n", [?MODULE, Event])
.

inplace_textbox_event(?COMPLETE_S(Name), Value) ->
    Percent = list_to_integer(
        case lists:last(Value) of
            $% ->
                lists:sublist(Value, length(Value) - 1);
            _  ->
                Value
        end)
    , iterate_wf:update_story_completion(Name, Percent)
    , lists:flatten(io_lib:format("~B%", [(Percent)]))
.

get_story(Name) ->
    [Story] = iterate_db:story(?Q_STORY(Name))
    , Story
.
