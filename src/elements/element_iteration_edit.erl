-module(element_iteration_edit).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("elements.hrl").
-include("events.hrl").
-include("iterate_records.hrl").

render(Record) ->
    Id       = Record#iteration_edit.iteration_id
    , Name     = Id ++ "_BacklogEditBox"
    , ButtonId = Name ++ "CloseButton"
    , Button   = #button{ 
        id=ButtonId
        , text="close"
        , actions=#event{ type=click%, override=true
                  , delegate=?MODULE
                  , postback=close}
    }
    , Desc = case Record#iteration_edit.desc of
        List when is_list(List) ->
            List;
        _                       ->
            "Description goes here"
    end
    , iterate_log:log_debug(wf:f("the description is: ~s~n", [Desc]))
    , Panel = #panel{ id=Name
        , body=[
           #my_inplace_textbox{ tag=?UPDATEDESC(Id),
                delegate=?MODULE, text=Desc }, #br{}
           , Button
        ]
    }
    , Panel
.

inplace_textbox_event(?UPDATEDESC(Name), Value) ->
    iterate_log:log_debug(wf:f("updating desc for ~s", [Name]))
    , case iterate_db:iteration(?Q_ITERATION(Name)) of
        %%{error, Msg} ->
            %% what do I do for this one?
        [B | []] ->
            B1 = B#iterations{desc=Value}
            , iterate_db:iteration(?UPDATEITER(B1))
    end
    , Value
.

event(close) ->
    element_iteration_panel:event(?REFRESH(undefined))
.

