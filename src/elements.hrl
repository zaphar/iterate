
-define(ITER_ELEMENT_BASE, data).

-record(my_inplace_textbox, {?CONTROL_BASE(element_my_inplace_textbox),
    text="", html_encode=true, start_mode=view, validators=[],
    delegate=?MODULE}).
-record(inplace_textarea, {?CONTROL_BASE(element_inplace_textarea),
    text="", html_encode=true, start_mode=view, validators=[],
    delegate=?MODULE, input_class}).
-record(delegated_droppable, {?ELEMENT_BASE(element_delegated_droppable),
    tag, body=[], accept_groups=all, active_class=active, hover_class=hover}).

%% elements for the building blocks of the widgets
-record(backlog, {?ELEMENT_BASE(element_backlog), ?ITER_ELEMENT_BASE,
    backlog_name, container}).
-record(backlog_edit, {?ELEMENT_BASE(element_backlog_edit), ?ITER_ELEMENT_BASE,
    backlog_id, desc, el_id}).
-record(iteration, {?ELEMENT_BASE(element_iteration), ?ITER_ELEMENT_BASE,
    iteration_name, container}).
-record(iteration_edit, {?ELEMENT_BASE(element_iteration_edit), ?ITER_ELEMENT_BASE,
    iteration_id, desc, el_id}).
-record(story_edit, {?ELEMENT_BASE(element_story_edit), ?ITER_ELEMENT_BASE,
    story_name, desc, sp}).
-record(story, {?ELEMENT_BASE(element_story), ?ITER_ELEMENT_BASE,
    story_name, backlog}).
-record(story_tasks, {?ELEMENT_BASE(element_story_tasks), ?ITER_ELEMENT_BASE,
    story}).
-record(flot_chart, {?ELEMENT_BASE(element_flot_chart), width, height, title
    , minx, maxx, miny, maxy, xticks, yticks
    , modex, modey, modex2, modey2
    , minx2, maxx2, miny2, maxy2, x2ticks, y2ticks
    , values, lines=true, points=true, selectmode="xy"
    , placeholder}).

%% elements for the high level widgets
-record(backlog_panel, {?ELEMENT_BASE(element_backlog_panel), data=undefined, filter="Filter Backlogs"}).
-record(iteration_panel, {?ELEMENT_BASE(element_iteration_panel), data=undefined}).
-record(story_panel, {?ELEMENT_BASE(element_story_panel), data=undefined}).
-record(report_panel, {?ELEMENT_BASE(element_report_panel), data=undefined}).

