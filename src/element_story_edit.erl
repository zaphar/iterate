-module(element_story_edit).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("elements.hrl").

-define(TAG(Type), tag={type, Type}).

render(ControlId, Record) ->
    PanelId          = wf:temp_id()
    , ButtonId       = wf:temp_id()
    , Name           = Record#story_edit.story_name
    , Desc           = Record#story_edit.desc
    , StoryPoints    = Record#story_edit.sp
    , Data  = [[Name, Desc, StoryPoints]]
    , Map   = [map_entry(PanelId ++ "_name", "text")
                , map_entry(PanelId ++ "_desc", "text") 
                , map_entry(PanelId ++ "_sp", "text")
              ]
    , Panel = #panel{ id=PanelId
                    , body=#bind{
                        data=Data
                        , map=Map
                        , body=[
                            #my_inplace_textbox{ id=PanelId ++ "_name"
                                , delegate=?MODULE
                                    , ?TAG('Name'), text=Name}, #br{}
                            , #my_inplace_textbox{id=PanelId ++ "_desc"
                                , delegate=?MODULE
                                    , ?TAG(desc), text=Desc}, #br{}
                            , #my_inplace_textbox{id=PanelId ++ "_sp"
                                , delegate=?MODULE
                                    , ?TAG(sp), text=StoryPoints}, #br{}
                            , #button{id=ButtonId, text="close"
                                , actions=#event{type=click
                                                 , delegate=element_story
                                                 , postback={remove, {story, Name}}}
                            }
                        ]
                    }
    }
    , element_panel:render(ControlId, Panel).

map_entry(Id, Attr) ->
    list_to_atom(Id ++ "@" ++ Attr).

%% TODO(jwall): bind this to do actual work
inplace_textbox_event(_Tag, Value) ->
    Value.

