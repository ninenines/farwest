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

-module(farwest_resource_h).

-export([init/2]).
-export([allowed_methods/2]).
-export([options/2]).
-export([resource_exists/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([provide_representation/2]).
-export([accept_representation/2]).

init(Req, Mod) ->
	%% @todo Probably worth normalizing the state here.
	%% @todo Probably put the Describe in Req itself instead. Then we can access it from everywhere.
	State = Mod:describe(),
	{cowboy_rest, Req#{resource => Mod}, State#{resource => Mod}}.

allowed_methods(Req, State) ->
	{farwest:resource_list_methods(State), Req, State}.

options(Req0, State=#{resource := Mod}) ->
	{ok, Links, Req1} = Mod:links(Req0),
	Req = set_link_headers(Req1, Links),
	{ok, Req, State}.

resource_exists(Req0, State=#{resource := Mod}) ->
	case Mod:locate(Req0) of
		{found, Req} ->
			{true, Req, State};
		{not_found, Req} ->
			{false, Req, State}
	end.

%% @todo We should always give the types for GET,
%% @todo plus the types for the current method? Or all methods?
content_types_provided(Req, State) ->
	{[{MT, provide_representation}
		|| MT <- farwest:resource_provides(State)], Req, State}.

content_types_accepted(Req, State) ->
	{[{MT, accept_representation}
		|| MT <- farwest:resource_accepts(State)], Req, State}.

provide_representation(Req0=#{media_type := MediaType}, State=#{resource := Mod}) ->
	case Mod:get(Req0) of
		{ok, Data, Req1} ->
			{ok, Links, Req2} = Mod:links(Req1),
			Alias = farwest:media_type_to_alias(MediaType, State),
			{ok, Body, Req3} = Mod:to_representation(Req2#{links => Links}, Alias, Data),
			Req4 = set_link_headers(Req3, Links),
			Req = set_variants_headers(Req4, State),
			{Body, Req, State}
	end.

%% @todo In addition to this alternate representations should use Variants.
%% It fits better into cowboy_rest for sure but let's do it here for now.
set_link_headers(Req0, Links0) ->
	AllLinks = lists:flatten([case Link of
		{Rel, Mod} when is_atom(Mod) ->
			Describe = Mod:describe(),
			{Type, Target} = case maps:get(uri_template, Describe, undefined) of
				undefined ->
					{uri, maps:get(uri, Mod:describe())};
				URITemplate ->
					{uri_template, URITemplate}
			end,
			MediaTypes = farwest:resource_provides(Describe),
			Attributes = case MediaTypes of
				[] -> [];
				_ -> [{<<"variants-06">>, cow_http_hd:variants([{<<"accept">>, MediaTypes}])}]
			end,
			#{
				target => Target,
				rel => atom_to_binary(Rel, utf8),
				attributes => Attributes,
				type => Type
			};
		{Rel, {template, URITemplate}} ->
			#{
				target => URITemplate,
				rel => atom_to_binary(Rel, utf8),
				attributes => [],
				type => uri_template
			};
		{Rel, URI} ->
			#{
				target => URI,
				rel => atom_to_binary(Rel, utf8),
				attributes => [],
				type => uri
			}
	end || Link <- Links0]),
	Req = case [L || L=#{type := uri} <- AllLinks] of
		[] -> Req0;
		Links ->
			cowboy_req:set_resp_header(<<"link">>, cow_link:link(Links), Req0)
	end,
	case [L || L=#{type := uri_template} <- AllLinks] of
		[] -> Req;
		LinkTemplates ->
			cowboy_req:set_resp_header(<<"farwest-link-templates">>, cow_link:link(LinkTemplates), Req)
	end.

%% @todo Also need to do language, encoding, session cookie, etc.
set_variants_headers(Req=#{media_type := MediaType}, State) ->
	MediaTypes = farwest:resource_provides(State),
	case MediaTypes of
		%% Doesn't vary.
		[] -> Req;
		[_] -> Req;
		%% @todo Need to remove the media type parameters.
		_ ->
			cowboy_req:set_resp_headers(#{
				<<"variants-06">> => cow_http_hd:variants([
					{<<"accept">>, MediaTypes}
				]),
				<<"variant-key-06">> => cow_http_hd:variant_key([[
					farwest:normalize_media_type(MediaType)
				]])
			}, Req)
	end.

accept_representation(Req0, State=#{resource := Mod}) ->
	Op = farwest:req_to_operation(Req0, State),
	case Mod:Op(Req0) of
		{ok, Req} ->
			{true, Req, State}
	end.
