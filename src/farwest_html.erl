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

-module(farwest_html).

-export([from_term/2]).

from_term(Req=#{links := Links}, Term) ->
	[
		header(),
		<<"<main>">>,
		links_to_html(Links),
		<<"<section id=\"contents\">">>,
		term_to_html(Term),
		<<"</section><section id=\"operations\">">>,
		operations_to_html(Req),
		<<"</section></main>">>,
		footer()
	].

%% @todo Link relation types can be a URI.
%% @todo Automatically generate 'alternate' links for each media type GET provides.
links_to_html([]) ->
	[];
links_to_html(Links) ->
	[
		<<"<nav><ul>">>,
		[case Link of
			{Rel, URIOrMod} ->
				URI = if
					is_atom(URIOrMod) ->
						maps:get(uri, URIOrMod:describe());
					true ->
						URIOrMod
				end,
				[<<"<li><a href=\"">>, URI, <<"\" rel=\"">>, atom_to_binary(Rel, utf8), <<"\">">>,
					URI, <<"</a></li>">>]
		end || Link <- Links],
		<<"</ul></nav>">>
	].

%% @todo Don't forget to htmlencode!
term_to_html([]) ->
	<<>>;
term_to_html(List) when is_list(List) ->
	[<<"<ul>">>, list_to_html(List), <<"</ul>">>];
term_to_html(Map) when is_map(Map) ->
	[<<"<dl>">>, map_to_html(Map), <<"</dl>">>];
term_to_html(Bin) when is_binary(Bin) ->
	[<<"<p>">>, Bin, <<"</p>">>];
term_to_html(Atom) when is_atom(Atom) ->
	[<<"<p>">>, atom_to_binary(Atom, utf8), <<"</p>">>];
term_to_html(Int) when is_integer(Int) ->
	integer_to_binary(Int);
term_to_html({'$fw_tab', []}) ->
	[];
term_to_html({'$fw_tab', _, []}) ->
	[];
term_to_html({'$fw_tab', List=[First|_]}) when is_map(First) ->
	Keys = maps:keys(First),
	term_to_html({'$fw_tab', Keys, List});
term_to_html({'$fw_tab', Keys, List=[First|_]}) when is_map(First) ->
	[
		<<"<table><thead><tr>">>,
		[[<<"<th>">>, Key, <<"</th>">>] || Key <- Keys],
		<<"</tr></thead><tbody>">>,
		[map_to_html_row(Map, Keys) || Map <- List],
		<<"</tbody></table>">>
	];
term_to_html({'$fw_tab', Cols, List=[First|_]}) when is_integer(Cols), is_tuple(First) ->
	Seq = lists:seq(1, Cols),
	[
		<<"<table><thead><tr>">>,
		[[<<"<th>">>, integer_to_binary(C), <<"</th>">>] || C <- Seq],
		<<"</tr></thead><tbody>">>,
		[tuple_to_html_row(Tuple, Seq) || Tuple <- List],
		<<"</tbody></table>">>
	];
term_to_html({'$fw_link', Rel, Target, Term}) ->
	[
		<<"<a rel=\"">>,
		atom_to_binary(Rel, utf8),
		<<"\" href=\"">>,
		Target,
		<<"\">">>,
		term_to_html(Term),
		<<"</a>">>
	];
term_to_html(Tuple) when is_tuple(Tuple) ->
	[<<"<ol>">>, list_to_html(tuple_to_list(Tuple)), <<"</ol>">>];
term_to_html(Ref) when is_reference(Ref) ->
	[<<"<p>">>, ref_to_list(Ref), <<"</p>">>];
term_to_html(Fun) when is_function(Fun) ->
	[<<"<p>">>, erlang:fun_to_list(Fun), <<"</p>">>];
term_to_html(Pid) when is_pid(Pid) ->
	[<<"<p>">>, pid_to_list(Pid), <<"</p>">>];
term_to_html(Port) when is_port(Port) ->
	[<<"<p>">>, erlang:port_to_list(Port), <<"</p>">>].

list_to_html(List) ->
	[[<<"<li>">>, term_to_html(E), <<"</li>">>] || E <- List].

map_to_html(Map) ->
	maps:fold(fun(Key, Value, Acc) ->
		[
			<<"<dt>">>, term_to_html(Key), <<"</dt>">>,
			<<"<dd>">>, term_to_html(Value), <<"</dd>">>
		|Acc]
	end, [], Map).

map_to_html_row(Map, Keys) ->
	[
		<<"<tr>">>,
		[[<<"<td>">>, term_to_html(maps:get(Key, Map)), <<"</td>">>] || Key <- Keys],
		<<"</tr>">>
	].

tuple_to_html_row(Tuple, Keys) ->
	[
		<<"<tr>">>,
		[[<<"<td>">>, term_to_html(try element(Key, Tuple) catch _:_ -> '' end), <<"</td>">>] || Key <- Keys],
		<<"</tr>">>
	].

operations_to_html(Req=#{resource := Mod}) ->
	#{operations := Ops} = Mod:describe(),
	maps:fold(fun(Op, OpInfo, Acc) ->
		Inputs = maps:get(input, OpInfo, []),
		[
			[
				operation_to_html(Req, Op, Alias)
			|| Alias <- Inputs]
		|Acc]
	end, [], Ops).

%% @todo Don't keep operations/methods hardcoded. Ideally
%% we would be able to describe operations entirely including
%% whether they are idempotent, take input, generally do output
%% and so on, and the method would be chosen based on that if
%% there are no suitable standard method.
operation_to_html(Req=#{resource := Mod}, Op, Alias) ->
	%% @todo Obviously we don't want to call get twice...
	{ok, Data0, _} = Mod:get(Req),
	%% @todo Obviously we should call to_representation or something.
	Data = io_lib:format("~0p", [Data0]),
	%% @todo Make operations definable.
	Method = case Op of
		put -> <<"PUT">>;
		process -> <<"POST">>;
		delete -> <<"DELETE">>
	end,
	[
		<<"<form method=\"">>,
		Method,
		<<"\" data-operation=\"">>,
		atom_to_binary(Op, utf8),
		<<"\" enctype=\"text/plain\"><legend>">>,
		atom_to_binary(Op, utf8),
		<<": ">>,
		farwest:resource_media_type(Mod, Alias),
		<<"</legend><textarea name=\"representation\" required>">>,
		Data,
		<<"</textarea><input type=\"submit\"/></form>">>
	].

header() ->
	<<
		"<!DOCTYPE html>"
		"<html lang=\"en\">"
		"<head>"
		"<meta charset=\"utf-8\">"
		"<title>Farwest auto-generated HTML</title>"
		"<link rel=\"stylesheet\" href=\"/farwest-static/farwest.css\">"
		"<script src=\"/farwest-static/farwest.js\"></script>"
		"</head>"
		"<body>"
	>>.

footer() ->
	<<
		"</body>"
		"</html>"
	>>.
