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

-module(farwest_client).

-export([interpret_response_headers/1]).

%% @todo prepare_request_headers

interpret_response_headers(Headers) when is_map(Headers) ->
	interpret_response_headers(maps:to_list(Headers));
interpret_response_headers(Headers) ->
	interpret(Headers, Headers, #{}).

interpret([{<<"content-type">>, ContentTypeHd}|Tail], All, Acc) ->
	interpret(Tail, All, Acc#{media_type => ContentTypeHd});
interpret([{<<"farwest-link-templates">>, LinkHd}|Tail], All, Acc) ->
	interpret(Tail, All, Acc#{link_templates => cow_link:parse_link(LinkHd)});
interpret([{<<"link">>, LinkHd}|Tail], All, Acc) ->
	interpret(Tail, All, Acc#{links => cow_link:parse_link(LinkHd)});
interpret([{<<"variants-06">>, VariantsHd}|Tail], All, Acc) ->
	Variants = cow_http_hd:parse_variants(VariantsHd),
	{_, VariantKeyHd} = lists:keyfind(<<"variant-key-06">>, 1, All),
	VariantKey = cow_http_hd:parse_variant_key(VariantKeyHd, length(Variants)),
	interpret(Tail, All, Acc#{
		variants => Variants,
		variant_key => VariantKey
	});
interpret([_|Tail], All, Acc) ->
	interpret(Tail, All, Acc);
interpret([], _, Acc) ->
	Acc.
