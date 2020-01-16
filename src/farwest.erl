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

-module(farwest).

-export([get_operations/0]).
-export([init_state/0]).
-export([list_resource_modules/1]).
-export([list_routes/1]).
-export([media_type_to_alias/2]).
-export([normalize_media_type/1]).
-export([register_operation/2]).
-export([req_to_operation/2]).
-export([resource_list_methods/1]).
-export([resource_list_ops/1]).
-export([resource_media_type/2]).
-export([resource_provides/1]).
-export([resource_accepts/1]).

get_operations() ->
	persistent_term:get(farwest_operations).

init_state() ->
	persistent_term:put(farwest_operations, #{
		get => #{
			idempotent => true,
			methods => [<<"GET">>, <<"HEAD">>],
			request_payload => none,
			safe => true
		},
		process => #{
			idempotent => false,
			methods => <<"POST">>,
			request_payload => representation,
			safe => false
		},
		put => #{
			idempotent => true,
			methods => <<"PUT">>,
			request_payload => representation,
			safe => false
		},
		delete => #{
			idempotent => true,
			methods => <<"DELETE">>,
			request_payload => none,
			safe => false
		}
	}).

list_resource_modules(App) ->
	{ok, Mod} = application:get_env(App, farwest_config_module),
	Mod:resource_modules().

list_routes(App) ->
	[begin
		case Mod:describe() of
			#{uri := URI, constraints := Constraints} ->
				{URI, Constraints, farwest_resource_h, Mod};
			#{uri := URI} ->
				{URI, farwest_resource_h, Mod}
		end
	end || Mod <- list_resource_modules(App)]
	++ [{"/farwest-static/[...]", cowboy_static, {priv_dir, farwest, "static/"}}].

media_type_to_alias(MediaType, Mod) when is_atom(Mod) ->
	media_type_to_alias(MediaType, Mod:describe());
media_type_to_alias(MediaType0, #{media_types := MediaTypes}) ->
	MediaType = normalize_media_type(MediaType0),
	maps:fold(fun
		(Alias, Types, []) ->
			case lists:member(MediaType, [normalize_media_type(T) || T <- Types]) of
				true -> Alias;
				false -> []
			end;
		(_, _, Acc) ->
			Acc
	end, [], MediaTypes).

%% @todo Use proper media type selection. Also handle params (optionally, not for variants).
normalize_media_type({Type, SubType, _Params}) ->
	iolist_to_binary([Type, $/, SubType]);
normalize_media_type(MediaType) ->
	iolist_to_binary(MediaType).

register_operation(Op, Def) ->
	Operations = get_operations(),
	false = maps:is_key(Op, Operations),
	persistent_term:put(farwest_operations, Operations#{Op => Def}).

%% @todo We need to differentiate between different POST operations.
req_to_operation(Req, Mod) when is_atom(Mod) ->
	req_to_operation(Req, Mod:describe());
req_to_operation(#{method := Method}, #{operations := Operations}) ->
	%% @todo This should probably be computed only once.
	MethodOp = maps:fold(fun
		(Op, #{methods := Ms}, Acc0) when is_list(Ms) ->
			lists:foldl(fun(M, Acc) -> Acc#{M => Op} end, Acc0, Ms);
		(Op, #{methods := M}, Acc) ->
			Acc#{M => Op}
	end, #{}, maps:with(maps:keys(Operations), get_operations())),
	#{Method := Op} = MethodOp,
	Op.

resource_list_methods(Mod) when is_atom(Mod) ->
	resource_list_methods(Mod:describe());
resource_list_methods(#{operations := Operations}) ->
	OpDefs = get_operations(),
	[<<"OPTIONS">>|lists:usort(lists:flatten([begin
		#{Op := #{methods := Methods}} = OpDefs,
		Methods
	end || Op <- maps:keys(Operations)]))].

%% @todo Is this necessary?
resource_list_ops(Mod) when is_atom(Mod) ->
	resource_list_ops(Mod:describe());
resource_list_ops(#{operations := Operations}) ->
	maps:keys(Operations).

%% Returns the *first* media type for the given alias.
resource_media_type(Mod, Alias) ->
	#{media_types := #{Alias := [MT|_]}} = Mod:describe(),
	normalize_media_type(MT).

resource_provides(Mod) when is_atom(Mod) ->
	resource_provides(Mod:describe());
resource_provides(Describe) ->
	do_media_types(Describe, output).

resource_accepts(Mod) when is_atom(Mod) ->
	resource_accepts(Mod:describe());
resource_accepts(Describe) ->
	do_media_types(Describe, input).

do_media_types(#{media_types := MediaTypes, operations := Operations}, Type) ->
	Aliases0 = maps:fold(fun(_Op, OpInfo, Acc) ->
		case OpInfo of
			#{Type := OpAliases} -> OpAliases ++ Acc;
			_ -> Acc
		end
	end, [], Operations),
	Aliases = lists:usort(Aliases0),
	maps:fold(fun(Alias, MTs, Acc) ->
		case lists:member(Alias, Aliases) of
			true -> [normalize_media_type(MT) || MT <- MTs] ++ Acc;
			false -> Acc
		end
	end, [], MediaTypes).
