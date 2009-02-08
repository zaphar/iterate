-module(element_story_panel).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("elements.hrl").

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
        body=[#label{text="Stories"}, #list{id=story_list, body=stories(Data) }]
    }
    , element_rounded_panel:render(ControlId, Panel).

stories([]) ->
    [];
stories([H|T]) ->
    [ #listitem{ body=#story{story_name=H} } | stories(T) ].
    
event(_) -> ok.
