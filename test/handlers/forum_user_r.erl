-module(forum_user_r).

-export([describe/0]).

describe() -> #{
	uri => "/users/:user"
}.
