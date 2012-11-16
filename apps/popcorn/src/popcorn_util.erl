-module(popcorn_util).
-author('marc.e.campbell@gmail.com').

-export([hour/0,
         last_24_hours/0,
         severity_to_number/1,
         number_to_severity/1]).

hour() -> integer_to_list(erlang:trunc(folsom_utils:now_epoch() / 3600)).

last_24_hours() ->
    lists:map(fun(Hours_Ago) ->
        integer_to_list(erlang:trunc(folsom_utils:now_epoch() / 3600) - Hours_Ago)
      end, lists:seq(0, 23)).

severity_to_number(<<"all">>) -> -1;
severity_to_number(<<"debug">>) -> 7;
severity_to_number(<<"info">>) -> 6;
severity_to_number(<<"notice">>) -> 5;
severity_to_number(<<"warn">>) -> 4;
severity_to_number(<<"error">>) -> 3;
severity_to_number(<<"critical">>) -> 2;
severity_to_number(<<"alert">>) -> 1;
severity_to_number(<<"emergency">>) -> 0;
severity_to_number(_) -> undefined.

number_to_severity(7) -> <<"debug">>;
number_to_severity(6) -> <<"info">>;
number_to_severity(5) -> <<"notice">>;
number_to_severity(4) -> <<"warn">>;
number_to_severity(3) -> <<"error">>;
number_to_severity(2) -> <<"critical">>;
number_to_severity(1) -> <<"alert">>;
number_to_severity(0) -> <<"emergency">>;
number_to_severity(_) -> <<"?">>.
