{application, iterate, [
	{description,  "Iterate! project management"},
	{mod, {iterate_app, []}},
	{env, [
		{platform, mochiweb}, %% {inets|yaws|mochiweb}
		{port, 8001},
		{session_timeout, 20},
		{sign_key, "SIGN_KEY"},
		{www_root, "./wwwroot"}
	]},
    {applications, [kernel, stdlib, mnesia]}
]}.
