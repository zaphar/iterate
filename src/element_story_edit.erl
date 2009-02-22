-module(element_story_edit).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("macros.hrl").
-include("elements.hrl").
-include("iterate_records.hrl").

render(ControlId, Record) ->
    PanelId          = wf:temp_id()
    , Name           = Record#story_edit.story_name
    , Story = get_story(Name)
    , Desc           = case Story#stories.desc of
        undefined ->
            "Fill in Description Here";
        D ->
            D
    end
    , StoryPoints    = case Story#stories.sp of
        undefined ->
            3;
        N         ->
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
                        , #button{text="complete"
                            , actions=#event{type=click
                                             , delegate=?MODULE
                                             , postback={complete, {story, Name}}}
                        }
                        , #button{text="close"
                            , actions=#event{type=click
                                             , delegate=element_story
                                             , postback={remove, {story, Name}}}
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
inplace_textbox_event(_Tag, Value) ->
    Value
.

event({complete, {story, Name}}) ->
    Story = get_story(Name)
    , Completed = story_util:complete(Story)
    , iterate_db:story({update, Completed})
    , element_story_panel:event(?SHOW_STORIES(Story#stories.backlog))
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

