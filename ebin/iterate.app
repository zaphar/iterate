{application, iterate, [
    {description,  "Iterate! project management"},
    {id, "iterate"},
    {vsn, "0.01.0"},
    {modules, [
        date_util,
        db_migrate_tools,
        element_backlog,
        element_backlog_edit,
        element_backlog_panel,
        element_delegated_droppable,
        element_flot_chart,
        element_inplace_textarea,
        element_iteration,
        element_iteration_edit,
        element_iteration_panel,
        element_my_inplace_textbox,
        element_notify,
        element_report_panel,
        element_story,
        element_story_edit,
        element_story_panel,
        element_story_tasks,
        iterate_app,
        iterate_db,
        iterate_log,
        iterate_report,
        iterate_stats,
        iterate_sup,
        iterate_wf,
        iteration_util,
        story_util,
        string_utils,
        web_index
     ]},
    {applications, []},
    {registered, [
        iterate,
        iterate_stats,
        iterate_logger,
        wf_session_sup,
        wf_session_server,
        quickstart_sup
    ]},
    {mod, {iterate_app, []}},
    {env, [
        {platform, mochiweb}, %% {inets|yaws|mochiweb}
        {port, 8001},
        {session_timeout, 20},
        {sign_key, "SIGN_KEY"},
        {www_root, "./wwwroot"},
        {log_file, "iterate.log"}
    ]}
]}.
