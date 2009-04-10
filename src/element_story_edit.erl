-module(element_story_edit).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("stats.hrl").
-include("events.hrl").
-include("elements.hrl").
-include("iterate_records.hrl").

render(ControlId, Record) ->
    PanelId = wf:temp_id()
    , Name = Record#story_edit.story_name
    , Story = get_story(Name)
    , TagString = string:join(get_tags(Name), ",")
    , TimeSpent = iterate_db:log_time(?Q_AMT(Name))
    , Desc = case Story#stories.desc of
        undefined ->
            "Fill in Description Here";
        [] ->
            "Fill in Description Here";
        D ->
            D
    end
    , StoryPoints = case Story#stories.sp of
        undefined ->
            3;
        0 ->
            "0";
        N ->
            N
    end
    , Panel = #panel{ id=PanelId
                    , body=[
                        "Description: ", #my_inplace_textbox{
                            delegate=?MODULE
                                , tag=?UPDATE_S_DESC(Name), text=Desc}, #br{}
                        , "Story Points: ", #my_inplace_textbox{
                            delegate=?MODULE
                                , tag=?UPDATESP(Name), text=StoryPoints}
                        , "Tags: ", #my_inplace_textbox{
                            delegate=?MODULE
                                , tag=?TAGCHANGE(Name), text=TagString}
                        , lists:flatten(
                            io_lib:format("Time Spent: ~.1f Hours ", [TimeSpent]))
                        , #br{}
                        , #link{text="Enter Time",
                                actions=#event{ delegate=?MODULE,
                                    postback=?UPDATE_T_LOG(Name)}}
                        , #br{}
                        , #button{text="complete"
                            , actions=#event{type=click
                                             , delegate=?MODULE
                                             , postback=?COMPLETE_S(Name)}
                        }
                        , #button{text="close"
                            , actions=#event{type=click
                                             , delegate=element_story
                                             , postback=?REMOVE_S_EL(Name)}
                        }
                    ]
    }
    , element_panel:render(ControlId, Panel).

map_entry(Id, Attr) when is_atom(Id) ->
    map_entry(atom_to_list(Id), Attr);
map_entry(Id, Attr) when is_atom(Attr) ->
    map_entry(Id, atom_to_list(Attr));
map_entry(Id, Attr) when is_list(Id), is_list(Attr) ->
    list_to_atom(Id ++ "@" ++ Attr).

inplace_textbox_event(?UPDATESP(Name), Value) ->
    io:format("updating story points for ~s~n", [Name])
    , iterate_stats:record(story, ?CHANGE_STAT(Name, sp, Value))
    , Story = get_story(Name)
    , Updated = Story#stories{sp=Value}
    , io:format("Original ~p~n", [Story])
    , io:format("updated: ~p~n", [Updated])   
    , Resulting = iterate_db:story({update, Updated})
    , io:format("Result: ~p~n", [Resulting])
    , Value;
inplace_textbox_event(?UPDATE_S_DESC(Name), Value) ->
    Story = get_story(Name)
    , iterate_stats:record(story, ?UPDATE_DESC_STAT(Name))
    , Updated = Story#stories{desc=Value}
    , iterate_db:story({update, Updated})
    , Value;
inplace_textbox_event(?TAGCHANGE(For), Value) ->
    TagList = string:tokens(Value, ",")
    , io:format("~p recieved tags:  [~p] for ~p~n", [?MODULE, TagList, For])
    , [iterate_db:tags(?NEWTAG(story, For, T)) || T <- TagList]
    , Value;
inplace_textbox_event(Tag, Value) ->
    event({inplace_event, Tag})
    , Value
.

event(?COMPLETE_S(Name)) ->
    Story = get_story(Name)
    , Backlog = Story#stories.backlog
    , iterate_stats:record(story, ?COMPLETE_STAT(Name, Backlog))
    , Completed = story_util:complete(Story)
    , iterate_db:story({update, Completed})
    , refresh_story_panel(Story)
    , element_iteration_panel:event(?REFRESH(undefined));
event(?UPDATE_T_LOG(Name)) ->
    Id = wf:temp_id()
    , PanelId = wf:temp_id()
    , Msg = io_lib:format("Enter hours for ~p ", [Name])
    , wf:flash(#panel{id=PanelId
        , body=[Msg
                , #textbox{ id=Id, actions=[#event{type=change, delegate=?MODULE
                        , postback=?NEWTIME(Name, Id, PanelId)}
                    , #event{type=click, actions="obj('me').select();"}]}
          ]
    })
    , wf:wire(Id, #event{type='timer', delay=300, actions="obj('me').focus();"});
event(?NEWTIME(Name, Id, PanelId)) ->
    [ValueString] = wf:q(Id)
    , Value = list_to_number(ValueString)
    , iterate_db:log_time(?UPDATETIME(Name, Value))
    , wf:update(PanelId, 
        io_lib:format("Updated time for ~p to ~p", [Name, Value]))
    %% TODO(jwall): update the iteration list also
    , element_story:event(?SHOW_S_EL(Name));
event(Event) ->
    io:format("~p recieved unknown event ~p~n", [?MODULE, Event])
.

refresh_story_panel(Story) ->
    {Type, Name} = story_util:get_type(Story)
    , element_story_panel:event(?SHOW_STORIES(Type, Name))
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
    {atomic, TagList} = iterate_db:tags(?Q_TAGS(story, Name))
    , case TagList of
        [] ->
            [?TVALUE(T) || T <- [?STAG(Name, "tag")] ];
        List ->
            [?TVALUE(T) || T <- List]
    end
.

