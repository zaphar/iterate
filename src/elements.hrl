
-record(backlog_el, {?ELEMENT_BASE(element_backlog_el), backlog_id, desc}).
-record(story_el,  {?ELEMENT_BASE(element_story_el), story_id, desc, sp}).
-record(task_el,   {?ELEMENT_BASE(element_task_el), task_id}).

