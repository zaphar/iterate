{application, iterate, [
    {description,  "Iterate! project management"},
    {id, "iterate"},
    {vsn, "0.01.0"},
    {modules, [
        element_backlog,
        element_backlog_edit,
        element_backlog_panel,
        element_delegated_droppable,
        element_iteration,
        element_iteration_edit,
        element_iteration_panel,
        element_my_inplace_textbox,
        element_story,
        element_story_edit,
        element_story_panel,
        iterate_app,
        iterate_db,
        iterate_stats,
        iterate_sup,
        iteration_util
     ]},
    {applications, []},
    {registered, [
        iterate,
        iterate_stats,
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
        {www_root, "./wwwroot"}
    ]}
]}.
