{application, 'farwest', [
	{description, "Framework for building RESTful HATEOAS-driven applications"},
	{vsn, "0.1.0"},
	{modules, ['farwest','farwest_app','farwest_bed','farwest_client','farwest_html','farwest_resource_h','farwest_router','farwest_sup']},
	{registered, [farwest_sup]},
	{applications, [kernel,stdlib,cowboy,gun]},
	{mod, {farwest_app, []}},
	{env, []}
]}.