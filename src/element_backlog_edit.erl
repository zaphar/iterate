-module(element_backlog_edit).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("elements.hrl").
-include("iterate_records.hrl").

-define(UPDATENAME(Name), {backlog, {update, {backlog_name, Name}}}).
-define(UPDATEDESC(Name), {backlog, {update, {backlog_desc, Name}}}).

render(ControlId, Record) ->
    %% TODO(jwall): change to temp ids wf:temp_id()
    Id = Record#backlog_edit.backlog_id,
    Name = Record#backlog_edit.backlog_id ++ "_BacklogEditBox",
    ButtonId = Name ++ "CloseButton",
    Button = #button{ id=ButtonId, text="close"},
    Panel = #panel{ id=Name,
        body=[
           #my_inplace_textbox{ tag=?UPDATENAME(Id),
                delegate=?MODULE, text=Record#backlog_edit.backlog_id }, #br{}
           , #my_inplace_textbox{ tag=?UPDATEDESC(Id),
                delegate=?MODULE, text=Record#backlog_edit.desc }, #br{}
           , Button
        ]
    },
    wf:wire(ButtonId, 
        #event{ type=click, delegate=element_backlog, 
            postback={remove, {backlog, Record#backlog_edit.backlog_id}}
        }
    ),
    element_panel:render(ControlId, Panel).

%% TODO(jwall): bind this to do actual work
inplace_textbox_event(?UPDATENAME(Name), Value) ->
    io:format("updating name for ~s", [Name]),
    case iterate_db:backlog({qry, Name}) of
        %%{error, Msg} ->
            %% what do I do for this one?
        [B | []] ->
            B1 = B#backlogs{backlog_name=Value},
            iterate_db:backlog({update, B1})
    end,
    Value;
inplace_textbox_event(?UPDATEDESC(Name), Value) ->
    io:format("updating desc for ~s", [Name]),
    case iterate_db:backlog({qry, Name}) of
        %%{error, Msg} ->
            %% what do I do for this one?
        [B | []] ->
            B1 = B#backlogs{desc=Value},
            iterate_db:backlog({update, B1})
    end,
    Value
.

