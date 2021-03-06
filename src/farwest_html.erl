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

from_term(Req, Term) ->
	[
		header(Req),
		<<"<main>">>,
		nav(Req),
		<<"<section id=\"contents\">">>,
		term_to_html(Term),
		<<"</section><section id=\"operations\">">>,
		operations_to_html(Req),
		<<"</section></main>">>,
		footer()
	].

%% @todo Link relation types can be a URI.
%% @todo Automatically generate 'alternate' links for each media type GET provides.
nav(#{links := []}) ->
	[];
nav(#{bindings := Bindings, links := Links}) ->
	[
		<<"<nav><ul>">>,
		[case Link of
			{Rel, URIOrMod} ->
				URI = if
					is_atom(URIOrMod) ->
						#{uri := URITemplate} = URIOrMod:describe(),
						%% We don't want to build links to child templates
						%% as we end up creating a link to self instead.
						case {Rel, string:find(URITemplate, <<"{">>)} of
							{child, nomatch} ->
								URITemplate;
							{child, _} ->
								child_template;
							_ ->
								cow_uri_template:expand(
									unicode:characters_to_binary(URITemplate),
									maps:fold(fun(Key, Value, Acc) ->
										Acc#{atom_to_binary(Key, utf8) => Value}
									end, #{}, Bindings))
						end;
					true ->
						URIOrMod
				end,
				case URI of
					child_template ->
						[];
					_ ->
						[<<"<li><a href=\"">>, enc_attr(URI),
							<<"\" rel=\"">>, enc_attr(atom_to_binary(Rel, utf8)), <<"\">">>,
							enc(URI), <<"</a></li>">>]
				end
		end || Link <- Links],
		<<"</ul></nav>">>
	].

term_to_html([]) ->
	<<>>;
term_to_html(List) when is_list(List) ->
	[<<"<ul>">>, list_to_html(List), <<"</ul>">>];
term_to_html(Map) when is_map(Map) ->
	[<<"<dl>">>, map_to_html(Map), <<"</dl>">>];
term_to_html(Bin) when is_binary(Bin) ->
	[<<"<p>">>, enc(Bin), <<"</p>">>];
term_to_html(Atom) when is_atom(Atom) ->
	[<<"<p>">>, enc(atom_to_binary(Atom, utf8)), <<"</p>">>];
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
		[[<<"<th>">>, enc(Key), <<"</th>">>] || Key <- Keys],
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
		enc_attr(atom_to_binary(Rel, utf8)),
		<<"\" href=\"">>,
		enc(Target),
		<<"\">">>,
		term_to_html(Term),
		<<"</a>">>
	];
term_to_html(Tuple) when is_tuple(Tuple) ->
	[<<"<ol>">>, list_to_html(tuple_to_list(Tuple)), <<"</ol>">>];
term_to_html(Ref) when is_reference(Ref) ->
	[<<"<p>">>, enc(ref_to_list(Ref)), <<"</p>">>];
term_to_html(Fun) when is_function(Fun) ->
	[<<"<p>">>, enc(erlang:fun_to_list(Fun)), <<"</p>">>];
term_to_html(Pid) when is_pid(Pid) ->
	[<<"<p>">>, enc(pid_to_list(Pid)), <<"</p>">>];
term_to_html(Port) when is_port(Port) ->
	[<<"<p>">>, enc(erlang:port_to_list(Port)), <<"</p>">>].

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

enc(String) when is_list(String) ->
	enc(unicode:characters_to_binary(String));
enc(Bin) when is_binary(Bin) ->
	<<case C of
		$< -> <<"&lt;">>;
		$> -> <<"&gt;">>;
		$$ -> <<"&amp;">>;
		_ -> <<C>>
	end || <<C>> <= Bin>>.

enc_attr(String) when is_list(String) ->
	enc_attr(unicode:characters_to_binary(String));
enc_attr(Bin) when is_binary(Bin) ->
	<<case C of
		$< -> <<"&lt;">>;
		$> -> <<"&gt;">>;
		$$ -> <<"&amp;">>;
		$" -> <<"&quot;">>;
		$' -> <<"&apos;">>;
		_ -> <<C>>
	end || <<C>> <= Bin>>.

operations_to_html(Req=#{resource_describe := #{operations := Ops}}) ->
	RegisteredOps = farwest:get_operations(),
	maps:fold(fun(Op, OpInfo, Acc) ->
		case RegisteredOps of
			%% @todo We also want to add safe operations (like get to different media types).
			#{Op := #{safe := true}} ->
				Acc;
			#{Op := #{methods := Methods, request_payload := none}} ->
				[operation_to_html(Op, Methods)|Acc];
			#{Op := #{methods := Methods, request_payload := representation}} ->
				Inputs = maps:get(input, OpInfo, []),
				[
					[
						operation_to_html(Req, Op, Methods, Alias)
					|| Alias <- Inputs]
				|Acc]
		end
	end, [], Ops).

operation_to_html(Op, Methods) when is_list(Methods) ->
	[[
		<<"<form method=\"">>,
		enc_attr(Method),
		<<"\" data-operation=\"">>,
		enc_attr(atom_to_binary(Op, utf8)),
		<<"\"><legend>">>,
		enc(atom_to_binary(Op, utf8)),
		<<"</legend><input type=\"submit\"/></form>">>
	] || Method <- Methods];
operation_to_html(Op, Method) ->
	operation_to_html(Op, [Method]).

operation_to_html(Req=#{resource := Mod, resource_describe := Describe},
		Op, Methods, Alias) when is_list(Methods) ->
	%% @todo Obviously we don't want to call get twice...
	{ok, Data0, _} = Mod:get(Req),
	%% @todo Obviously we should call to_representation or something.
	Data = io_lib:format("~0p", [Data0]),
	[[
		<<"<form method=\"">>,
		enc_attr(Method),
		<<"\" data-operation=\"">>,
		enc_attr(atom_to_binary(Op, utf8)),
		%% We need to cheat for the media type because browsers don't support much.
		<<"\" data-media-type=\"">>,
		enc_attr(farwest:resource_media_type(Describe, Alias)),
		<<"\" enctype=\"text/plain\"><legend>">>,
		enc(atom_to_binary(Op, utf8)),
		<<": ">>,
		enc(farwest:resource_media_type(Describe, Alias)),
		<<"</legend><textarea name=\"representation\" required>">>,
		enc(Data),
		<<"</textarea><input type=\"submit\"/></form>">>
	] || Method <- Methods];
operation_to_html(Req, Op, Method, Alias) ->
	operation_to_html(Req, Op, [Method], Alias).

header(Req) ->
	[
		<<
			"<!DOCTYPE html>"
			"<html lang=\"en\">"
			"<head>"
			"<meta charset=\"utf-8\">"
			"<title>Farwest auto-generated HTML</title>"
			"<link rel=\"stylesheet\" href=\"/farwest-static/farwest.css\">"
		>>,
		header_links(Req),
		<<
			"<script src=\"/farwest-static/farwest.js\"></script>"
			"</head>"
			"<body>"
		>>
	].

%% @todo Link relation types can be a URI.
%% @todo Automatically generate 'alternate' links for each media type GET provides.
header_links(#{bindings := Bindings, links := Links}) ->
	[case Link of
		{Rel, Mod} when is_atom(Mod) ->
			Describe = #{uri := URITemplate} = Mod:describe(),
			%% We don't want to build links to child templates
			%% as we end up creating a link to self instead.
			URI = case {Rel, string:find(URITemplate, <<"{">>)} of
				{child, nomatch} ->
					URITemplate;
				{child, _} ->
					child_template;
				_ ->
					cow_uri_template:expand(
						unicode:characters_to_binary(URITemplate),
						maps:fold(fun(Key, Value, Acc) ->
							Acc#{atom_to_binary(Key, utf8) => Value}
						end, #{}, Bindings))
			end,
			case URI of
				child_template ->
					[];
				_ ->
					Provides = farwest:resource_provides(Describe),
					[
						[<<"<link rel=\"">>, enc_attr(atom_to_binary(Rel, utf8)),
							<<"\" type=\"">>, enc_attr(MT),
							<<"\" href=\"">>, enc_attr(URI), <<"\">">>]
					|| MT <- Provides]
			end;
		{Rel, URI} ->
			[<<"<link rel=\"">>, enc_attr(atom_to_binary(Rel, utf8)),
				<<"\" href=\"">>, enc_attr(URI), <<"\">">>]
	end || Link <- Links].

footer() ->
	<<
		"</body>"
		"</html>"
	>>.
