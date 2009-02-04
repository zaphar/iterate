
-record(backlog_edit, {?ELEMENT_BASE(element_backlog_el), backlog_id, desc}).
-record(backlog,      {?ELEMENT_BASE(element_backlog), backlog_name}).
-record(story_edit,   {?ELEMENT_BASE(element_story_edit), story_id, desc, sp}).
-record(story,        {?ELEMENT_BASE(element_story), story_id, desc, sp}).
-record(task,         {?ELEMENT_BASE(element_task), task_id}).
-record(task_edit,    {?ELEMENT_BASE(element_task_edit), task_id}).

