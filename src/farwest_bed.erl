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

operations_to_bed(Req=#{resource := Mod}) ->
	#{operations := Ops} = Mod:describe(),
	lists:flatten(maps:fold(fun(Op, OpInfo, Acc) ->
		Inputs = maps:get(input, OpInfo, []),
		[
			[
				operation_to_bed(Req, Op, Alias)
			|| Alias <- Inputs]
		|Acc]
	end, [], Ops)).

%% @todo Don't keep operations/methods hardcoded. Ideally
%% we would be able to describe operations entirely including
%% whether they are idempotent, take input, generally do output
%% and so on, and the method would be chosen based on that if
%% there are no suitable standard method.
operation_to_bed(#{resource := Mod}, Op, Alias) ->
	%% @todo Make operations definable.
	Method = case Op of
		put -> <<"PUT">>;
		process -> <<"POST">>;
		delete -> <<"DELETE">>
	end,
	#{
		operation => Op,
		method => Method,
		media_type => farwest:resource_media_type(Mod, Alias)
	}.
