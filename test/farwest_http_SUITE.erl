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

-module(farwest_http_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).

all() ->
	[{group, farwest}].

groups() ->
	Tests = ct_helper:all(?MODULE),
	[{farwest, [parallel], Tests}].

init_per_suite(Config) ->
	{ok, _} = application:ensure_all_started(farwest_demo),
	OriginPort = ranch:get_port(clear_farwest_demo),
	[{origin_port, OriginPort}|Config].

end_per_suite(_) ->
	ok = application:stop(farwest_demo).

%% Tests.

get_entry_point(Config) ->
	doc("Get the entry point resource."),
	{ok, ConnPid} = gun:open("localhost", config(origin_port, Config)),
	{ok, _} = gun:await_up(ConnPid),
	StreamRef = gun:get(ConnPid, "/", #{
		<<"accept">> => <<"application/x-bed;q=1, */*;q=0.1">>
	}),
	{response, nofin, 200, _} = gun:await(ConnPid, StreamRef),
	{ok, Body} = gun:await_body(ConnPid, StreamRef),
	_ = binary_to_term(Body),
	gun:close(ConnPid).

map_service(Config) ->
	doc("Build a map of the server by following the links "
		"and extracting relevant information about each resource."),
	{ok, ConnPid} = gun:open("localhost", config(origin_port, Config)),
	{ok, _} = gun:await_up(ConnPid),
	Map = do_map_path(ConnPid, <<"/">>, #{}),
	ct:pal("~p~n", [Map]),
	gun:close(ConnPid).

do_map_path(ConnPid, Path, Map0) ->
	case Map0 of
		#{Path := _} ->
			Map0;
		_ ->
			StreamRef = gun:head(ConnPid, Path, #{
				<<"accept">> => <<"application/x-bed;q=1, */*;q=0.1">>
			}),
			{response, fin, 200, Headers} = gun:await(ConnPid, StreamRef),
			Info = farwest_client:interpret_response_headers(Headers),
			Map = Map0#{Path => Info},
			case Info of
				#{links := Links} ->
					do_map_links(ConnPid, Links, Map);
				_ ->
					Map
			end
	end.

do_map_links(_, [], Map) ->
	Map;
do_map_links(ConnPid, [#{target := Target}|Tail], Map) ->
	%% @todo Don't follow external links.
	#{path := Path} = uri_string:parse(Target),
	do_map_links(ConnPid, Tail, do_map_path(ConnPid, Path, Map)).
