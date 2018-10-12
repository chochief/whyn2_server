{application, 'whyn', [
	{description, "whatsyourna.me"},
	{vsn, "0.1.0"},
	{modules, ['socket_handler','whyn_app','whyn_auth','whyn_bot','whyn_gps','whyn_live','whyn_mnesia','whyn_sup','wss_handler']},
	{registered, [whyn_sup]},
	{applications, [kernel,stdlib,mnesia,cowboy,jsx]},
	{mod, {whyn_app, []}},
	{env, []}
]}.