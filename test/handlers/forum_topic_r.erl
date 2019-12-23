-module(forum_topic_r).

-export([describe/0]).

-export([links/1]).
-export([locate/1]).
-export([get/1]).
-export([to_representation/3]).

describe() -> #{
	uri => "/topics/:topic",
	media_types => #{
		html => ["text/html"],
		json => ["application/json"],
		problem_json => ["application/problem+json"],
		vendor_json => ["application/vnd.ninenines.topic+json"],
		urlencoded => ["application/x-www-form-urlencoded"]
	},
	operations => #{
		get => #{output => [html, vendor_json]},
		put => #{input => [vendor_json, urlencoded]},
		process => #{
			input => [urlencoded],
			output => [json, problem_json]
		},
		delete => #{}
	}
}.

links(Req) ->
	{ok,[
		{parent, "/topics"}
	], Req}.

locate(Req) ->
	{found, Req}.

get(Req) ->
	{ok, [
		#{title => <<"Hello world!">>, contents => <<"This is my first post!">>},
		#{title => <<"Rules. Read first!">>, contents => <<"No rules here!">>}
	], Req}.

to_representation(Req, html, Data) ->
	{ok, farwest_html:from_term(Req, Data), Req};
to_representation(Req, vendor_json, _Data) ->
	{ok, <<"{\"type\": \"topic\"}">>, Req}.
