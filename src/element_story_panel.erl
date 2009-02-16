-module(element_story_panel).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("elements.hrl").
-include("iterate_records.hrl").

render(ControlId, Record) ->
    %% TODO(jwall): change to temp ids wf:temp_id()
    PanelId = wf:temp_id()
    , Data    = case Record#story_panel.data of
        undefined ->
            [];
        D when is_list(D) ->
            D
    end
    , Panel = #rounded_panel{ id=PanelId,
        body=[#label{text="Stories"}, 
              #list{id=story_list, body=stories(Data) }]
    }
    , element_rounded_panel:render(ControlId, Panel).

stories([]) ->
    [];
stories([H|T]) ->
    [ #listitem{ body=#story{story_name=H} } | stories(T) ].

story(Record) ->
    Name = Record#stories.story_name
    , #listitem{ id=Name,
        body=#story{story_name=Name }
    }.

%% showing stories
event({show, {stories, Name}}) ->
    wf:update(story_list, [story(SName) || SName <- iterate_db:stories(Name)]);
event(_) -> ok.
