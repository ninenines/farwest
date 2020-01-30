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

-module(farwest_bed).

-export([from_term/2]).

%% We don't add the resource-wide links here because they are
%% provided directly in the response headers and there's no
%% value repeating them in the body.
from_term(Req, Term) ->
	term_to_binary(#{
		data => Term,
		operations => operations_to_bed(Req)
	}).

operations_to_bed(Req=#{resource_describe := #{operations := Ops}}) ->
	RegisteredOps = farwest:get_operations(),
	lists:flatten(maps:fold(fun(Op, OpInfo, Acc) ->
		case RegisteredOps of
			%% @todo We also want to add safe operations (like get to different media types).
			#{Op := #{safe := true}} ->
				Acc;
			#{Op := #{methods := Methods, request_payload := none}} ->
				[operation_to_bed(Op, Methods)|Acc];
			#{Op := #{methods := Methods, request_payload := representation}} ->
				Inputs = maps:get(input, OpInfo, []),
				[
					[
						operation_to_bed(Req, Op, Methods, Alias)
					|| Alias <- Inputs]
				|Acc]
		end
	end, [], Ops)).

operation_to_bed(Op, Methods) when is_list(Methods) ->
	[#{
		operation => Op,
		method => Method
	} || Method <- Methods];
operation_to_bed(Op, Method) ->
	operation_to_bed(Op, [Method]).

operation_to_bed(#{resource_describe := Describe}, Op, Methods, Alias) when is_list(Methods) ->
	[#{
		operation => Op,
		method => Method,
		media_type => farwest:resource_media_type(Describe, Alias)
	} || Method <- Methods];
operation_to_bed(Req, Op, Method, Alias) ->
	operation_to_bed(Req, Op, [Method], Alias).
