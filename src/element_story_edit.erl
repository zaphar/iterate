-module(element_story_edit).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("events.hrl").
-include("elements.hrl").
-include("iterate_records.hrl").

render(ControlId, Record) ->
    PanelId = wf:temp_id()
    , Name = Record#story_edit.story_name
    , Story = get_story(Name)
    , TimeLog = iterate_db:log_time({qry, Name})
    , TagString = string:join(get_tags(Name), ",")
    , TimeSpent = lists:foldl(fun({T, _TS}, T2) -> T + T2 end, 0, TimeLog#time_log.t_series) 
    , Desc = case Story#stories.desc of
        undefined ->
            "Fill in Description Here";
        D ->
            D
    end
    , StoryPoints = case Story#stories.sp of
        undefined ->
            3;
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
                            io_lib:format("Time Spent: ~.10B Hours ", [TimeSpent]))
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

%% TODO(jwall): bind this to do actual work
inplace_textbox_event(?UPDATESP(Name), Value) ->
    io:format("updating story points for ~s~n", [Name]),    
    Story = get_story(Name),
    Updated = Story#stories{sp=Value},
    io:format("Original ~p~n", [Story]),    
    io:format("updated: ~p~n", [Updated]),    
    Resulting = iterate_db:story({update, Updated}),
    io:format("Result: ~p~n", [Resulting]),    
    Value;
inplace_textbox_event(?UPDATE_S_DESC(Name), Value) ->
    Story = get_story(Name)
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
    , Completed = story_util:complete(Story)
    , iterate_db:story({update, Completed})
    , element_story_panel:event(?SHOW_STORIES(Story#stories.backlog));
event(?UPDATE_T_LOG(Name)) ->
    Id = wf:temp_id()
    , PanelId = wf:temp_id()
    , Msg = io_lib:format("Enter hours for ~p", [Name])
    , wf:flash(#panel{id=PanelId
        , body=[Msg
                , #textbox{ id=Id, actions=#event{type=change, delegate=?MODULE,
                    postback=?NEWTIME(Name, Id, PanelId)}}
          ]
    });
event(?NEWTIME(Name, Id, PanelId)) ->
    [ValueString] = wf:q(Id)
    , Value = list_to_integer(ValueString)
    , iterate_db:log_time({Name, Value})
    , wf:update(PanelId, 
        io_lib:format("Updated time for ~p to ~p", [Name, Value]))
    , element_story:event(?SHOW_S_EL(Name));
event(Event) ->
    io:format("~p recieved unknown event ~p~n", [?MODULE, Event])
.

get_story(Name) ->
    case iterate_db:story({qry, {story, Name}}) of
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

