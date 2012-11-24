-module(popcorn_util).
-author('marc.e.campbell@gmail.com').

-include("include/popcorn.hrl").

-export([hour/0,
         last_24_hours/0,
         severity_to_number/1,
         number_to_severity/1,
         all_severity_numbers/0,
         format_log_message/1,
         opt/2]).

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

all_severity_numbers() -> lists:seq(0, 7).

opt(<<>>, Default)      -> Default;
opt(undefined, Default) -> Default;
opt(Value, _)           -> Value.

format_log_message(Log_Message) ->
  UTC_Timestamp = calendar:now_to_universal_time({Log_Message#log_message.timestamp div 1000000000000, 
                                                 Log_Message#log_message.timestamp div 1000000 rem 1000000,
                                                 Log_Message#log_message.timestamp rem 1000000}),
  {{Year, Month, Day}, {Hour, Minute, Second}} = UTC_Timestamp,
  Formatted_DateTime = lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Year, Month, Day, Hour, Minute, Second])),
  Formatted_Time     = lists:flatten(io_lib:format("~2.10.0B:~2.10.0B:~2.10.0B", [Hour, Minute, Second])),

  Find_More_Html     = "<strong>Filter current list to show only messages with matching:</strong><br /><br />" ++
                      "<label class='checkbox popover-label'><input type='checkbox'>Severity: " ++ binary_to_list(popcorn_util:number_to_severity(Log_Message#log_message.severity)) ++ "</label>" ++
                      "<label class='checkbox popover-label'><input type='checkbox'>Module: " ++ opt(binary_to_list(Log_Message#log_message.log_module), "Not set") ++ "</label>" ++
                      "<label class='checkbox popover-label'><input type='checkbox'>Function: " ++ opt(binary_to_list(Log_Message#log_message.log_function), "Not set") ++ "</label>" ++
                      "<label class='checkbox popover-label'><input type='checkbox'>Line: " ++ opt(binary_to_list(Log_Message#log_message.log_line), "?") ++ " in " ++ opt(binary_to_list(Log_Message#log_message.log_module), "not set") ++ "</label>" ++
                      "<label class='checkbox popover-label'><input type='checkbox'>Pid: " ++ opt(binary_to_list(Log_Message#log_message.log_pid), "Not set") ++ "</label><br />" ++
                      "<button class='btn btn-mini' type='button'>Apply Filter</button>",

  [{'time',             Formatted_Time},
   {'datetime',         Formatted_DateTime},
   {'find_more_html',   Find_More_Html},
   {'log_module',       opt(binary_to_list(Log_Message#log_message.log_module), "Unknown")},
   {'log_function',     opt(binary_to_list(Log_Message#log_message.log_function), "Unknown")},
   {'log_line',         opt(binary_to_list(Log_Message#log_message.log_line), "??")},
   {'log_pid',          opt(binary_to_list(Log_Message#log_message.log_pid), "?")},
   {'message_severity', binary_to_list(popcorn_util:number_to_severity(Log_Message#log_message.severity))},
   {'message',          binary_to_list(Log_Message#log_message.message)}].
