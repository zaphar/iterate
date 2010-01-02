-module(element_story).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("events.hrl").
-include("elements.hrl").
-include("iterate_records.hrl").

reflect() ->
    record_info(fields, story)
.

render(_ControlId, Record) ->
    TopId = wf:temp_id()
    , StoryId = Record#story.story_name
    , [Story] = iterate_db:story(?Q_STORY(StoryId))
    , Panel = #panel{id=TopId, class=story_class(Story)
            , body=story_element_panel(TopId, Story)}
    , element_panel:render(TopId, Panel).

story_element_panel(TopId, Story) ->
    PanelId = wf:temp_id()
    , Header = story_element_header(TopId, Story)
    , Operations = story_element_operations(TopId, Story)
    , EditPanel = story_edit_element(TopId, Story)
    , #panel{id=PanelId, body=Header ++ Operations ++ EditPanel}
.

story_element_header(_TopId, Story) ->
    StoryId = Story#stories.story_name
    , DraggableId = wf:temp_id()
    , OrderElId = wf:temp_id()
    , Order = story_util:order(Story)
    , TitleEditBox = #my_inplace_textbox{delegate=?MODULE
        , class="inline"
        , text=Story#stories.story_title
        , tag={edit_title, StoryId}}
    , DragHandle = #draggable{ class="inline", tag=StoryId
        , id=DraggableId
        , body="[drag me] " }
    , OrderElement = #textbox{id=OrderElId
        , actions=#event{
            type=change
            , delegate=?MODULE
            , postback=?ORDEDIT(StoryId, OrderElId)
        }
        , style="width: 20px;"
        , text=lists:flatten(
            io_lib:format("~.10B", [Order]))}
    , [DragHandle, OrderElement, " - ", TitleEditBox, "<br />"]
.

story_element_operations(TopId, Story) ->
    EditId = wf:temp_id()
    , Percent = story_util:completion(Story)
    , StoryId = Story#stories.story_name
    , EditLink = #link{text="edit", id=EditId
        , actions=#event{ type=click, delegate=?MODULE
                        , postback=?SHOW_S_EL(StoryId)
        }
    }
    , DeleteLink = #link{text="delete"
        , actions=#event{ 
            type=click
            , delegate=?MODULE
            , postback=?DELETE_S_EL(StoryId, TopId)
        }
    }
    , CompleteLink = #link{text="complete"
        , actions=#event{type=click
                         , delegate=element_story_edit
                         , postback=?COMPLETE_S(StoryId)}
    }
    , PercentBox = #my_inplace_textbox{ class="inline", delegate=?MODULE,
        text=io_lib:format("~.10B%", [Percent]),
        tag=?COMPLETE_S(StoryId)
    }
    , ["[ ", EditLink, " | "
            , DeleteLink, " | "
            , CompleteLink, " ] - "
            , PercentBox]
.

story_edit_element(TopId, Story) ->
    element_story_edit:render(TopId
        , #story_edit{story_name=Story#stories.story_name})
.

story_class(Story) ->
    BaseClass = "panel_element story_element"
    , case story_util:is_complete(Story) of
        true ->
            BaseClass ++ " complete ";
        false ->
            BaseClass
    end
.

event(?SHOW_S_EL(StoryId)) ->
    wf:update(StoryId ++ "_target",
        #story_edit{story_name=StoryId, desc="A description", sp="3"});
event(?REMOVE_S_EL(StoryId)) ->
    wf:update(StoryId ++ "_target", "");
event(?DELETE_S_EL(StoryId, Id)) ->
    Records = iterate_db:story(?Q_STORY(StoryId))
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
    , event(?REMOVE_S_EL(StoryId))
    , ok;
event(?ORDEDIT(StoryId, Id)) ->
    Value = case wf:q(Id) of
        [] ->
            0;
        [Found] ->
            Found
    end
    , iterate_log:log_debug(wf:f("updating order of ~p to ~p"
        , [StoryId, Value]))
    , Story = get_story(StoryId)
    , Updated = story_util:set_order(Story, Value)
    , iterate_db:story({update, Updated})
    , iterate_log:log_debug(wf:f("changing order ~p to ~p"
        , [story_util:order(Story), story_util:order(Updated)]))
    , Value;
event(Event) -> 
    iterate_log:log_debug(wf:f("~p received ~p event~n", [?MODULE, Event]))
.

inplace_textbox_event(?COMPLETE_S(StoryId), Value) ->
    Percent = list_to_integer(
        case lists:last(Value) of
            $% ->
                lists:sublist(Value, length(Value) - 1);
            _  ->
                Value
        end)
    , iterate_wf:update_story_completion(StoryId, Percent)
    , element_iteration_panel:event(?REFRESH(bogus_id))
    , lists:flatten(io_lib:format("~B%", [(Percent)]));
inplace_textbox_event({edit_title, Id}, Value) ->
    Story = get_story(Id)
    , iterate_db:story({update, Story#stories{story_title=Value}})
    , Value
.

get_story(Name) ->
    case iterate_db:story(?Q_STORY(Name)) of
        {error, Msg} ->
            throw(Msg);
        [Result | []] ->
            Result;
        _ ->
            throw("whoah what was that?")
    end
.

get_tags(Name) ->
    [ binary_to_list(B) || B <- iterate_wf:get_story_tags(Name)]
.

list_to_number("." ++ L) ->
    list_to_number("0." ++ L);
list_to_number(L) ->
    case string:to_float(L) of
        {error, no_float} ->
            case string:to_integer(L) of
                {error, _} ->
                    throw({error, not_a_number});
                {Int, []} ->
                    Int
            end;
        {Float, []} ->
            Float
    end
.
