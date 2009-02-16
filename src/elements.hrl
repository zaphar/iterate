
-define(ITER_ELEMENT_BASE, data).

-record(my_inplace_textbox, {?CONTROL_BASE(element_my_inplace_textbox), text="", html_encode=true, start_mode=view, validators=[], delegate=?MODULE}).

%% elements for the building blocks of the widgets
-record(backlog_edit, {?ELEMENT_BASE(element_backlog_edit), ?ITER_ELEMENT_BASE,
    backlog_id, desc, el_id}).
-record(backlog,      {?ELEMENT_BASE(element_backlog),      ?ITER_ELEMENT_BASE,
    backlog_name}).
-record(story_edit,   {?ELEMENT_BASE(element_story_edit),   ?ITER_ELEMENT_BASE,
    story_name, desc, sp}).
-record(story,        {?ELEMENT_BASE(element_story),        ?ITER_ELEMENT_BASE,
    story_name}).
-record(task,         {?ELEMENT_BASE(element_task),         ?ITER_ELEMENT_BASE,
    task_name}).
-record(task_edit,    {?ELEMENT_BASE(element_task_edit),    ?ITER_ELEMENT_BASE,
    task_name}).

%% elements for the high level widgets
-record(backlog_panel, {?ELEMENT_BASE(element_backlog_panel), data=undefined}).
-record(story_panel, {?ELEMENT_BASE(element_story_panel), data=undefined}).
