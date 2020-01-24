%% Copyright (c) 2019, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(farwest_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [doc/1]).

all() ->
	[{group, farwest}].

groups() ->
	Tests = ct_helper:all(?MODULE),
	[{farwest, [parallel], Tests}].

init_per_suite(Config) ->
	ok = application:load(farwest),
	application:set_env(farwest, farwest_config_module, ?MODULE),
	Config.

end_per_suite(_) ->
	ok = application:unload(farwest).

%% Farwest configuration.

resource_modules() ->
	get('$resource_modules').

%% Tests.

list_resource_modules(_) ->
	Mods = [a, b, c],
	put('$resource_modules', Mods),
	Mods = farwest:list_resource_modules(farwest),
	ok.

list_routes(_) ->
	put('$resource_modules', [forum_topics_r, forum_topic_r, forum_users_r, forum_user_r]),
	[
		{"/", farwest_resource_h, forum_topics_r},
		{"/topics/{topic}", farwest_resource_h, forum_topic_r},
		{"/users", farwest_resource_h, forum_users_r},
		{"/users/{user}", farwest_resource_h, forum_user_r},
		{"/farwest-static{/path_info*}", cowboy_static, {priv_dir, farwest, "static/"}}
	] = farwest:list_routes(farwest),
	ok.

resource_provides(_) ->
	[
		<<"application/vnd.ninenines.topic+json">>,
		<<"application/problem+json">>,
		<<"application/json">>,
		<<"text/html">>
	] = farwest:resource_provides(forum_topic_r),
	ok.
