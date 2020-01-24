%% Copyright (c) 2020, Loïc Hoguin <essen@ninenines.eu>
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

%% Routing middleware using URI Templates as routes.
%%
%% Resolve the handler to be used for the request based on the
%% routing information found in the <em>dispatch</em> environment value.
%% When found, the handler module and associated data are added to
%% the environment as the <em>handler</em> and <em>handler_opts</em> values
%% respectively.
%%
%% If the route cannot be found, processing stops with either
%% a 400 or a 404 reply.
-module(farwest_router).
-behaviour(cowboy_middleware).

-export([compile/1]).
-export([execute/2]).

-type bindings() :: #{atom() => any()}.
-export_type([bindings/0]).

-type route_match() :: '_' | iodata().
-type route_path() :: {Path::route_match(), Handler::module(), Opts::any()}
	| {Path::route_match(), cowboy:fields(), Handler::module(), Opts::any()}.
-type route_rule() :: {Host::route_match(), Paths::[route_path()]}
	| {Host::route_match(), cowboy:fields(), Paths::[route_path()]}.
-type routes() :: [route_rule()].
-export_type([routes/0]).

-type dispatch_match() :: '_' | cow_uri_template:uri_template().
-type dispatch_path() :: {dispatch_match(), cowboy:fields(), module(), any()}.
-type dispatch_rule() :: {Host::dispatch_match(), cowboy:fields(), Paths::[dispatch_path()]}.
-opaque dispatch_rules() :: [dispatch_rule()].
-export_type([dispatch_rules/0]).

-include_lib("cowlib/include/cow_inline.hrl").
-include_lib("cowlib/include/cow_parse.hrl").

-spec compile(routes()) -> dispatch_rules().
compile(Routes) ->
	compile(Routes, []).

compile([], Acc) ->
	lists:reverse(Acc);
compile([{Host, Paths}|Tail], Acc) ->
	compile([{Host, [], Paths}|Tail], Acc);
compile([{HostMatch, Fields, Paths}|Tail], Acc) ->
	Host = compile_host(HostMatch),
	PathRules = compile_paths(Paths, []),
	compile(Tail, [{Host, Fields, PathRules}|Acc]).

compile_host('_') ->
	'_';
compile_host(URITemplate) when is_list(URITemplate) ->
	compile_host(unicode:characters_to_binary(URITemplate));
%% We remove a single leading dot if any.
%% The hosts .example.org and example.org are considered the same.
compile_host(<<".",URITemplate/bits>>) ->
	atomize_var_names(cow_uri_template:parse(URITemplate));
compile_host(URITemplate) ->
	atomize_var_names(cow_uri_template:parse(URITemplate)).

compile_paths([], Acc) ->
	lists:reverse(Acc);
compile_paths([{PathMatch, Handler, Opts}|Tail], Acc) ->
	compile_paths([{PathMatch, [], Handler, Opts}|Tail], Acc);
compile_paths([{'_', Fields, Handler, Opts}|Tail], Acc) ->
	compile_paths(Tail, [{'_', Fields, Handler, Opts}] ++ Acc);
compile_paths([{PathMatch, Fields, Handler, Opts}|Tail], Acc) when is_list(PathMatch) ->
	compile_paths([{unicode:characters_to_binary(PathMatch), Fields, Handler, Opts}|Tail], Acc);
compile_paths([{<<"*">>, Fields, Handler, Opts}|Tail], Acc) ->
	compile_paths(Tail, [{<<"*">>, Fields, Handler, Opts}|Acc]);
compile_paths([{URITemplate = <<$/, _/bits>>, Fields, Handler, Opts}|Tail], Acc) ->
	compile_paths(Tail, [{atomize_var_names(cow_uri_template:parse(URITemplate)), Fields, Handler, Opts}|Acc]);
compile_paths([{PathMatch, _, _, _}|_], _) ->
	error({badarg, "The following route MUST begin with a slash: "
		++ binary_to_list(PathMatch)}).

atomize_var_names([]) ->
	[];
atomize_var_names([{expr, Op, [{Modifier, Name}]}|Tail]) ->
	[{expr, Op, [{Modifier, binary_to_atom(Name, utf8)}]}|atomize_var_names(Tail)];
atomize_var_names([Bin|Tail]) ->
	[Bin|atomize_var_names(Tail)].

-spec execute(Req, Env)
	-> {ok, Req, Env} | {stop, Req}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().
execute(Req=#{host := Host, path := Path0}, Env=#{dispatch := Dispatch0}) ->
	Dispatch = case Dispatch0 of
		{persistent_term, Key} -> persistent_term:get(Key);
		_ -> Dispatch0
	end,
	%% No need to handle <<"*">> specially because normalize doesn't change it.
	case uri_string:normalize(Path0) of
		{error, invalid_uri, _} ->
			{stop, cowboy_req:reply(400, Req)};
		Path ->
			case match_host(Dispatch, Host, Path) of
				{ok, Handler, HandlerOpts, Bindings} ->
					{ok, Req#{
						host_info => maps:get(host_info, Bindings, undefined),
						path_info => maps:get(path_info, Bindings, undefined),
						bindings => maps:without([host_info, path_info], Bindings)
					}, Env#{
						handler => Handler,
						handler_opts => HandlerOpts
					}};
				{error, notfound, host} ->
					{stop, cowboy_req:reply(400, Req)};
				{error, notfound, path} ->
					{stop, cowboy_req:reply(404, Req)}
			end
	end.

%% Match the host and path against their corresponding URI Templates.
%%
%% Matches are ungreedy, that is, expressions will match
%% as little as possible if they are followed by a binary.
%% Matching will stop when the next binary matches as well
%% (or when an invalid character or end of string is encountered).
%%
%% The special value '_' (instead of a URI Template) will always
%% match the full host/path. The path has a special <<"*">> value
%% as well for "OPTIONS *" requests.
%%
%% When a result is found, this function will return the handler module and
%% options found in the dispatch list and a key-value list of bindings.

match_host([], _, _) ->
	{error, notfound, host};
%% If the host is '_' then there can be no constraints.
match_host([{'_', [], ListOfPaths}|_Tail], _, Path) ->
	match_path(ListOfPaths, Path, #{});
%% When we have a leading label_expansion_with_dot_prefix binding for
%% the host name, we add a leading dot to the hostname even if one was
%% not already there, because the leading dot is optional.
match_host([{URITemplate=[{expr, label_expansion_with_dot_prefix, _}|_],
		Fields, ListOfPaths}|Tail], Host= <<C,_/bits>>, Path) when C =/= $. ->
	case match_uri_template(URITemplate, Fields, <<$.,Host/binary>>, #{}) of
		false ->
			match_host(Tail, Host, Path);
		{true, Bindings} ->
			match_path(ListOfPaths, Path, Bindings)
	end;
match_host([{URITemplate, Fields, ListOfPaths}|Tail], Host, Path) ->
	case match_uri_template(URITemplate, Fields, Host, #{}) of
		false ->
			match_host(Tail, Host, Path);
		{true, Bindings} ->
			match_path(ListOfPaths, Path, Bindings)
	end.

match_path([], _, _) ->
	{error, notfound, path};
%% If the path is '_' then there can be no constraints.
match_path([{'_', [], Handler, Opts}|_Tail], _, Bindings) ->
	{ok, Handler, Opts, Bindings};
match_path([{<<"*">>, _, Handler, Opts}|_Tail], <<"*">>, Bindings) ->
	{ok, Handler, Opts, Bindings};
match_path([_|Tail], <<"*">>, Bindings) ->
	match_path(Tail, <<"*">>, Bindings);
match_path([{URITemplate, Fields, Handler, Opts}|Tail], Path, Bindings0) ->
	case match_uri_template(URITemplate, Fields, Path, Bindings0) of
		false ->
			match_path(Tail, Path, Bindings0);
		{true, Bindings} ->
			{ok, Handler, Opts, Bindings}
	end.

match_uri_template([], _, <<>>, Bindings) ->
	{true, Bindings};
match_uri_template([], _, _, _) ->
	false;
match_uri_template([{expr, simple_string_expansion, [{no_modifier, Name}]}|Tail],
		Fields, URI0, Bindings) ->
	{Value, URI} = take_unreserved(URI0, next_match(Tail), <<>>),
	match_value(Tail, Fields, URI, Bindings, Name, Value);
match_uri_template([{expr, path_segment_expansion, [{explode_modifier, Name}]}|Tail],
		Fields, URI0, Bindings) ->
	{Value, URI} = take_path_segments(URI0, next_match(Tail), []),
	match_value(Tail, Fields, URI, Bindings, Name, Value);
match_uri_template([{expr, label_expansion_with_dot_prefix, [{explode_modifier, Name}]}|Tail],
		Fields, URI0, Bindings) ->
	Next = next_match(Tail),
	Len = byte_size(Next),
	case URI0 of
		<<Next:Len/binary,URI/bits>> when Next =/= <<>> ->
			match_value(tl(Tail), Fields, URI, Bindings, Name, []);
		_ ->
			{Value, URI} = take_dot_segments(URI0, Next, []),
			match_value(Tail, Fields, URI, Bindings, Name, Value)
	end;
match_uri_template([Bin|Tail], Fields, URI0, Bindings) ->
	Len = byte_size(Bin),
	case URI0 of
		<<Bin:Len/binary, URI/bits>> ->
			match_uri_template(Tail, Fields, URI, Bindings);
		_ ->
			false
	end.

next_match([Bin|_]) when is_binary(Bin) -> Bin;
next_match(_) -> <<>>.

match_value(Tail, Fields, URI, Bindings, Name, Value0) ->
	case Bindings of
		%% Name is the wildcard, discard the value.
		_ when Name =:= '_' ->
			match_uri_template(Tail, Fields, URI, Bindings);
		%% Value already exists in the bindings. Check constraints and then
		%% confirm that the produced value matches the existing value.
		#{Name := ExistingValue} ->
			case match_constraints(Name, Value0, Fields) of
				{true, ExistingValue} ->
					match_uri_template(Tail, Fields, URI, Bindings);
				_ ->
					false
			end;
		%% New binding. Check constraints.
		_ ->
			case match_constraints(Name, Value0, Fields) of
				{true, Value} ->
					match_uri_template(Tail, Fields, URI, Bindings#{Name => Value});
				false ->
					false
			end
	end.

match_constraints(_, Value, []) ->
	{true, Value};
match_constraints(Name, Value0, [Field|Tail]) when element(1, Field) =:= Name ->
	Constraints = element(2, Field),
	case cowboy_constraints:validate(Value0, Constraints) of
		{ok, Value} ->
			match_constraints(Name, Value, Tail);
		{error, _} ->
			false
	end;
%% We skip fields where only the atom is specified as there
%% are no constraints to check. We also skip fields for other
%% names because we check constraints one field at a time.
match_constraints(Name, Value, [_|Tail]) ->
	match_constraints(Name, Value, Tail).

%% @todo Maybe have a function with ungreedy check and one without.
%% The version with ungreedy check can just skip the next element
%% instead of matching it twice (here and above).
take_unreserved(<<C,R/bits>>, Next, Acc0)
		when ?IS_ALPHANUM(C) or (C =:= $-) or (C =:= $.) or (C =:= $_) or (C =:= $~) ->
	Acc = <<Acc0/binary,C>>,
	Len = byte_size(Next),
	case R of
		<<Next:Len/binary,_/bits>> when Next =/= <<>> ->
			{Acc, R};
		_ ->
			take_unreserved(R, Next, Acc)
	end;
take_unreserved(<<$%,H,L,R/bits>>, Next, Acc0) ->
	Acc = <<Acc0/binary,?UNHEX(H,L)>>,
	Len = byte_size(Next),
	case R of
		<<Next:Len/binary,_/bits>> when Next =/= <<>> ->
			{Acc, R};
		_ ->
			take_unreserved(R, Next, Acc)
	end;
take_unreserved(R, _, Acc) ->
	{Acc, R}.

take_path_segments(<<$/,R0/bits>>, Next, Acc) ->
	{Segment, R} = take_unreserved(R0, Next, <<>>),
	Len = byte_size(Next),
	case R of
		<<Next:Len/binary,_/bits>> when Next =/= <<>> ->
			{lists:reverse([Segment|Acc]), R};
		_ ->
			take_path_segments(R, Next, [Segment|Acc])
	end;
take_path_segments(R, _, Acc) ->
	{lists:reverse(Acc), R}.

take_dot_segments(<<$.,R0/bits>>, Next, Acc) ->
	{Segment, R} = take_unreserved_no_dot(R0, Next, <<>>),
	Len = byte_size(Next),
	case R of
		<<Next:Len/binary,_/bits>> when Next =/= <<>> ->
			{lists:reverse([Segment|Acc]), R};
		_ ->
			take_dot_segments(R, Next, [Segment|Acc])
	end;
take_dot_segments(R, _, Acc) ->
	{lists:reverse(Acc), R}.

take_unreserved_no_dot(<<C,R/bits>>, Next, Acc0)
		when ?IS_ALPHANUM(C) or (C =:= $-) or (C =:= $_) or (C =:= $~) ->
	Acc = <<Acc0/binary,C>>,
	Len = byte_size(Next),
	case R of
		<<Next:Len/binary,_/bits>> when Next =/= <<>> ->
			{Acc, R};
		_ ->
			take_unreserved_no_dot(R, Next, Acc)
	end;
take_unreserved_no_dot(<<$%,H,L,R/bits>>, Next, Acc0) ->
	Acc = <<Acc0/binary,?UNHEX(H,L)>>,
	Len = byte_size(Next),
	case R of
		<<Next:Len/binary,_/bits>> when Next =/= <<>> ->
			{Acc, R};
		_ ->
			take_unreserved_no_dot(R, Next, Acc)
	end;
take_unreserved_no_dot(R, _, Acc) ->
	{Acc, R}.

-ifdef(TEST).
compile_test_() ->
	Tests = [
		{
			[{'_', [
				{'_', h, o}
			]}],
			[{'_', [], [
				{'_', [], h, o}
			]}]
		},
		{
			[{"cowboy.example.org", [
				{"/", ha, oa},
				{"/path/to/resource", hb, ob}
			]}],
			[{[<<"cowboy.example.org">>], [], [
				{[<<"/">>], [], ha, oa},
				{[<<"/path/to/resource">>], [], hb, ob}
			]}]
		},
		%% This router treats /resource and /resource/ differently
		%% while cowboy_router was treating them as the same resource.
		{
			[{'_', [{"/path/to/resource/", h, o}]}],
			[{'_', [], [{[<<"/path/to/resource/">>], [], h, o}]}]
		},
		{
			[{".cowboy.example.org", [{'_', h, o}]}],
			[{[<<"cowboy.example.org">>], [], [{'_', [], h, o}]}]
		},
		{
			[{"{subdomain}.example.org", [
				{"/hats/{name}/prices", h, o}
			]}],
			[{[{expr, simple_string_expansion, [{no_modifier, subdomain}]}, <<".example.org">>], [], [
				{[
					<<"/hats/">>,
					{expr, simple_string_expansion, [{no_modifier, name}]},
					<<"/prices">>
				], [], h, o}
			]}]
		},
		{
			[{"ninenines.{_}", [
				{"/hats/{_}", h, o}
			]}],
			[{[<<"ninenines.">>, {expr, simple_string_expansion, [{no_modifier, '_'}]}], [], [
				{[<<"/hats/">>, {expr, simple_string_expansion, [{no_modifier, '_'}]}], [], h, o}
			]}]},
		{
			[{'_', [{"/hats/{page}/{number}", h, o}]}],
			[{'_', [], [
				{[
					<<"/hats/">>,
					{expr, simple_string_expansion, [{no_modifier, page}]},
					<<"/">>,
					{expr, simple_string_expansion, [{no_modifier, number}]}
				], [], h, o}]}]
		},
		{
			[{"{.host_info*}.ninenines.eu", [{"/hats{/path_info*}", h, o}]}],
			[
				{[
					{expr, label_expansion_with_dot_prefix, [{explode_modifier, host_info}]},
					<<".ninenines.eu">>
				], [], [
					{[
						<<"/hats">>,
						{expr, path_segment_expansion, [{explode_modifier, path_info}]}
					], [], h, o}
				]}
			]
		}
	],
	[{lists:flatten(io_lib:format("~p", [Rt])),
		fun() -> Rs = compile(Rt) end} || {Rt, Rs} <- Tests].

match_host_test_() ->
	Dispatch = compile([
		{"www.{_}.ninenines.eu", [
			{"/users/{_}/mails", match_any_subdomain_users, []}
		]},
		{"ninenines.eu", [
			{"/users/{id}/friends", match_ninenines_eu_users_friends, []},
			{'_', match_ninenines_eu, []}
		]},
		{"ninenines.{var}", [
			{"/threads/{var}", match_duplicate_vars, [we, {expect, two}, var, here]}
		]},
		{"erlang.{ext}", [
			{'_', match_erlang_ext, []}
		]},
		{'_', [
			{"/users/{id}/friends", match_users_friends, []},
			{'_', match_any, []}
		]}
	]),
	Tests = [
		{<<"any">>, <<"/">>, {ok, match_any, [], #{}}},
		{<<"www.any.ninenines.eu">>, <<"/users/42/mails">>,
			{ok, match_any_subdomain_users, [], #{}}},
		{<<"www.ninenines.eu">>, <<"/users/42/mails">>,
			{ok, match_any, [], #{}}},
		{<<"www.ninenines.eu">>, <<"/">>,
			{ok, match_any, [], #{}}},
		{<<"www.any.ninenines.eu">>, <<"/not_users/42/mails">>,
			{error, notfound, path}},
		{<<"ninenines.eu">>, <<"/">>,
			{ok, match_ninenines_eu, [], #{}}},
		{<<"ninenines.eu">>, <<"/users/42/friends">>,
			{ok, match_ninenines_eu_users_friends, [], #{id => <<"42">>}}},
		{<<"erlang.fr">>, '_',
			{ok, match_erlang_ext, [], #{ext => <<"fr">>}}},
		{<<"any">>, <<"/users/444/friends">>,
			{ok, match_users_friends, [], #{id => <<"444">>}}}
	],
	[{lists:flatten(io_lib:format("~p, ~p", [H, P])), fun() ->
		R = match_host(Dispatch, H, P)
	end} || {H, P, R} <- Tests].

match_info_test_() ->
	Dispatch = compile([
		{"www.ninenines.eu", [
			{"/pathinfo/is/next{/path_info*}", match_path, []}
		]},
		{"{.host_info*}.ninenines.eu", [
			{'_', match_any, []}
		]}
	]),
	Tests = [
		{<<"ninenines.eu">>, <<"/">>,
			{ok, match_any, [], #{host_info => []}}},
		{<<"bugs.ninenines.eu">>, <<"/">>,
			{ok, match_any, [], #{host_info => [<<"bugs">>]}}},
		{<<"cowboy.bugs.ninenines.eu">>, <<"/">>,
			{ok, match_any, [], #{host_info => [<<"cowboy">>, <<"bugs">>]}}},
		{<<"www.ninenines.eu">>, <<"/pathinfo/is/next">>,
			{ok, match_path, [], #{path_info => []}}},
		{<<"www.ninenines.eu">>, <<"/pathinfo/is/next/path_info">>,
			{ok, match_path, [], #{path_info => [<<"path_info">>]}}},
		{<<"www.ninenines.eu">>, <<"/pathinfo/is/next/foo/bar">>,
			{ok, match_path, [], #{path_info => [<<"foo">>, <<"bar">>]}}}
	],
	[{lists:flatten(io_lib:format("~p, ~p", [H, P])), fun() ->
		R = match_host(Dispatch, H, P)
	end} || {H, P, R} <- Tests].

match_constraints_test() ->
	Dispatch1 = compile([{'_', [
		%% This router treats /resource and /resource/ differently
		%% while cowboy_router was treating them as the same resource.
		{"/path/{value}", [{value, int}], h, o},
		{"/path/{value}/", [{value, int}], h, o}
	]}]),
	{ok, h, o, #{value := 123}} = match_host(Dispatch1, <<"ninenines.eu">>, <<"/path/123">>),
	{ok, h, o, #{value := 123}} = match_host(Dispatch1, <<"ninenines.eu">>, <<"/path/123/">>),
	{error, notfound, path} = match_host(Dispatch1, <<"ninenines.eu">>, <<"/path/NaN/">>),
	Dispatch2 = compile([{'_', [
		{"/path/{username}", [{username, fun(_, Value) ->
				case cowboy_bstr:to_lower(Value) of
					Value -> {ok, Value};
					_ -> {error, not_lowercase}
				end
			end}], h, o}
	]}]),
	{ok, h, o, #{username := <<"essen">>}} = match_host(Dispatch2, <<"ninenines.eu">>, <<"/path/essen">>),
	{error, notfound, path} = match_host(Dispatch2, <<"ninenines.eu">>, <<"/path/ESSEN">>),
	ok.

match_same_bindings_test() ->
	Dispatch1 = compile([{"{same}.{same}", [{'_', h, o}]}]),
	{ok, h, o, #{same := <<"eu">>}} = match_host(Dispatch1, <<"eu.eu">>, <<"/">>),
	{error, notfound, host} = match_host(Dispatch1, <<"ninenines.eu">>, <<"/">>),
	Dispatch2 = compile([{"{user}.ninenines.eu", [
		%% This router treats /resource and /resource/ differently
		%% while cowboy_router was treating them as the same resource.
		{"/path/{user}", h, o},
		{"/path/{user}/", h, o}
	]}]),
	{ok, h, o, #{user := <<"essen">>}} = match_host(Dispatch2, <<"essen.ninenines.eu">>, <<"/path/essen">>),
	{ok, h, o, #{user := <<"essen">>}} = match_host(Dispatch2, <<"essen.ninenines.eu">>, <<"/path/essen/">>),
	{error, notfound, path} = match_host(Dispatch2, <<"essen.ninenines.eu">>, <<"/path/notessen">>),
	Dispatch3 = compile([{'_', [{"/{same}/{same}", h, o}]}]),
	{ok, h, o, #{same := <<"path">>}} = match_host(Dispatch3, <<"ninenines.eu">>, <<"/path/path">>),
	{error, notfound, path} = match_host(Dispatch3, <<"ninenines.eu">>, <<"/path/to">>),
	%% @todo Add a test case with bindings that transform the value.
	%% I suspect this test case would fail when using cowboy_router,
	%% but should work with farwest_router.
	ok.
-endif.
