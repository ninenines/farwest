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
-export([delete_resource/2]).

init(Req, Mod) ->
	%% @todo Probably worth normalizing the describe here.
	{cowboy_rest, Req#{
		resource => Mod,
		resource_describe => Mod:describe()
	}, #{}}.

allowed_methods(Req=#{resource_describe := Describe}, State) ->
	{farwest:resource_list_methods(Describe), Req, State}.

options(Req0=#{resource := Mod}, State) ->
	{ok, Links, Req1} = Mod:links(Req0),
	Req = set_link_headers(Req1, Links),
	{ok, Req, State}.

resource_exists(Req0=#{resource := Mod}, State) ->
	case Mod:locate(Req0) of
		{found, Req} ->
			{true, Req, State};
		{not_found, Req} ->
			{false, Req, State}
	end.

%% @todo We should always give the types for GET,
%% @todo plus the types for the current method? Or all methods?
content_types_provided(Req=#{resource_describe := Describe}, State) ->
	{[{MT, provide_representation}
		|| MT <- farwest:resource_provides(Describe)], Req, State}.

content_types_accepted(Req=#{resource_describe := Describe}, State) ->
	{[{MT, accept_representation}
		|| MT <- farwest:resource_accepts(Describe)], Req, State}.

provide_representation(Req0=#{resource := Mod, resource_describe := Describe,
		media_type := MediaType}, State) ->
	case Mod:get(Req0) of
		{ok, Data, Req1} ->
			{ok, Links, Req2} = Mod:links(Req1),
			Alias = farwest:media_type_to_alias(MediaType, Describe),
			{ok, Body, Req3} = Mod:to_representation(Req2#{links => Links}, Alias, Data),
			Req4 = set_link_headers(Req3, Links),
			Req = set_variants_headers(Req4),
			{Body, Req, State}
	end.

%% @todo In addition to this alternate representations should use Variants.
%% It fits better into cowboy_rest for sure but let's do it here for now.
set_link_headers(Req0, Links0) ->
	AllLinks = lists:flatten([case Link of
		{Rel, Mod} when is_atom(Mod) ->
			Describe = #{uri := URI} = Mod:describe(),
			{Type, Target} = case string:find(URI, <<"{">>) of
				nomatch ->
					{uri, URI};
				_ ->
					{uri_template, URI}
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
set_variants_headers(Req=#{resource_describe := Describe, media_type := MediaType}) ->
	MediaTypes = farwest:resource_provides(Describe),
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

accept_representation(Req0=#{resource := Mod}, State) ->
	Op = farwest:req_to_operation(Req0),
	case Mod:Op(Req0) of
		{ok, Req} ->
			{true, Req, State}
	end.

delete_resource(Req0=#{resource := Mod}, State) ->
	Op = farwest:req_to_operation(Req0),
	case Mod:Op(Req0) of
		{ok, Req} ->
			{true, Req, State}
	end.
