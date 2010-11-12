-module(element_backlog_edit).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("stats.hrl").
-include("elements.hrl").
-include("events.hrl").
-include("iterate_records.hrl").

-import(iterate_element_utils, [normalize_id/1]).

render_element(Record) ->
    Id       = Record#backlog_edit.backlog_id
    , IdStripped = normalize_id(Id)
    , ElId     = Record#backlog_edit.el_id
    , Name     = IdStripped ++ "_BacklogEditBox"
    , ButtonId = Name ++ "CloseButton"
    , Button   = #button{ 
        id=ButtonId
        , text="close"
        , actions=#event{ type=click
                  , delegate=element_backlog
                  , postback=?REMOVE_B_EL(Id, ElId)
                }
    }
    , Desc = case Record#backlog_edit.desc of
        List when is_list(List) ->
            List;
        _                       ->
            "Description goes here"
    end
    , Msg = wf:f("the description is: ~s~n", [Desc])
    , iterate_log:log_debug(Msg)
    , #panel{ class="edit_panel", id=Name
        , body=[
           #my_inplace_textbox{ tag=?UPDATEDESC(Id),
                delegate=?MODULE, text=Desc }, #br{}
           , Button
        ]
    }
.

inplace_textbox_event(?UPDATEDESC(Name), Value) ->
    Msg = wf:f("updating desc for ~s", [Name])
    , iterate_log:log_debug(Msg)
    , iterate_stats:record(backlog, ?UPDATE_DESC_STAT(Name))
    , case iterate_db:backlog({qry, Name}) of
        %%{error, Msg} ->
            %% what do I do for this one?
        [B | []] ->
            B1 = B#backlogs{desc=Value},
            iterate_db:backlog({update, B1})
    end,
    Value
.

