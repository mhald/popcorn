-module(popcorn_util).
-author('marc.e.campbell@gmail.com').

-export([hour/0,
         last_24_hours/0]).

hour() -> integer_to_list(erlang:trunc(folsom_utils:now_epoch() / 3600)).

last_24_hours() ->
    lists:map(fun(Hours_Ago) ->
        integer_to_list(erlang:trunc(folsom_utils:now_epoch() / 3600) - Hours_Ago)
      end, lists:seq(0, 23)).

