-module(element_story_tasks).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("stats.hrl").
-include("events.hrl").
-include("elements.hrl").
-include("iterate_records.hrl").

render(_, T) ->
    Story = T#story_tasks.story
    , {Id, Panel} = build_tasks(Story)
    , element_panel:render(Id, Panel)
.

build_tasks(Name) ->
    PanelId = wf:temp_id()
    , Content = build_rows(Name, PanelId)
    , {PanelId, #panel{id=PanelId, body=Content}}
.

build_rows(Name, PanelId) ->
    {atomic, TaskList} = iterate_db:task(?Q_STORY_TASKS(Name))
    , io:format("our PanelId for tasks still is: ~p", [PanelId])
    , Tasks = case TaskList of
        [] ->
            [];
        List ->
            [ build_task_row(T, PanelId) || T <- List ]
    end
    , NewTask = #link{text="new task", actions=#event{
        delegate=?MODULE, postback={new, {task, "Enter Name Here"}
            , PanelId, Name}}}
    , Refresh = #link{text="refresh", actions=#event{
        delegate=?MODULE, postback={refresh, PanelId, Name}}}
    , HeaderFooter = [#tableheader{body=[NewTask, " | ", Refresh]}]
    , Content = HeaderFooter ++ Tasks
    , #table{class=tasks, rows=Content }
.

build_task_row(T, PanelId) ->
    TextBox = #my_inplace_textbox{text=T#tasks.task_name
        , delegate=?MODULE, tag={update, T#tasks.id, T#tasks.story_name}}
    , CBId = wf:temp_id()
    , CheckBox = #checkbox{id=CBId, checked=T#tasks.complete 
        , actions=#event{type=change, delegate=?MODULE
            , postback=?COMPLETE_TASK(CBId, T#tasks.id)}}
    , Delete = #link{text="delete", actions=#event{type=click
        , delegate=?MODULE
        , postback=?DELETE_TASK(T#tasks.id, PanelId, T#tasks.story_name)}}
    , #tablerow{id=T#tasks.id, cells=[
        #tablecell{body=CheckBox}
        , #tablecell{body=TextBox}
        , #tablecell{body=Delete}
        ]
    }
.

event({new, {task, Name}, Id, Story}) ->
    case iterate_db:task(?C_NEW_TASK(Story, Name)) of
        {atomic, _TId} ->
            event({refresh, Id, Story});
        Err ->
            throw(Err)
    end;
event({refresh, Id, Story}) ->
    wf:update(Id, build_rows(Story, Id));
event(?COMPLETE_TASK(CBId, Id)) ->
    Value = case wf:q(CBId) of
        ["on"] ->
            true;
        [] ->
            false
    end
    , io:format("The Checkbox is: ~p~n", [Value])
    , case get_task(Id) of
        {ok, Task} ->
            iterate_db:task(?U_TASK(Task#tasks{complete=Value}));
        {abort, Err} ->
            wf:flash(wf:f(
                "whoops it looks we encountered an error [~p] while updating this task ~p"
                , [Err, Id]))
    end;
event(?DELETE_TASK(Id, PanelId, Story)) ->
    case iterate_db:task(?D_TASK(Id)) of
        {atomic, ok} ->
            wf:update(PanelId, build_rows(Story, PanelId));
        {abort, Err} ->
            wf:flash(wf:f("whoops we appear to have encountered an error [~p] while deleting this task ~p",
                [Err, Id]))
    end;
event(E) ->
    io:format("warning: encountered unhandled event ~p", [E])
.

inplace_textbox_event({update, Id, For}, Value) ->
    case iterate_db:task(?Q_TASK(Id)) of
        {atomic, []} ->
            iterate_db:task(?U_TASK(#tasks{id=Id, task_name=Value, story_name=For}))
            , Value;
        {atomic, [T | []]} ->
            iterate_db:task(?U_TASK(T#tasks{task_name=Value}))
            , Value
    end
.

get_task(Id) ->
    case iterate_db:task(?Q_TASK(Id)) of
        {atomic, [Task | []]} ->
           {ok, Task};
        {atomic, []} ->
            {abort, no_such_task};
        Err ->
            {abort, Err}
    end
.

