-module(view_login).
-author('marc.e.campbell@gmail.com').

-include("include/popcorn.hrl").

-export([head_includes/0]).

-spec head_includes() -> list().
head_includes() -> popcorn_util:head_includes().

