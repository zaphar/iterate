{application, iterate, [
	{description,  "Iterate! project management"},
	{id, "iterate"},
	{vsn, "0.01.0"},
	{modules, [iterate_app,
		element_backlog,
		element_backlog_edit,
		element_backlog_panel,
		element_my_inplace_textbox,
		element_story,
		element_story_edit,
		element_story_panel,
		iterate_db,
		web_index
	 ]},
	{applications, []},
	{registered, [
		iterate,
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
