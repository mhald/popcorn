-module(view_node_log).
-author('marc.e.campbell@gmail.com').

-include("include/popcorn.hrl").

-export([log_messages/1]).

-spec log_messages(dict()) -> list().
log_messages(Context) ->
    Node_Name = mustache:get(node_name, Context),
    Severity  = mustache:get(severity, Context),

    ?POPCORN_DEBUG_MSG("Node Name = ~p\n Severity = ~p", [Node_Name, Severity]),

    Prefix           = <<"_popcorn__">>,
    Metric_Name      = binary_to_atom(<<Prefix/binary, Node_Name/binary>>, latin1),

    lists:map(fun(Log_Message) ->
        {Log_Time, Message_Elements} = lists:nth(1, Log_Message),
        UTC_Timestamp = calendar:now_to_universal_time({Log_Time div 1000000000000, 
                                                        Log_Time div 1000000 rem 1000000,
                                                        Log_Time rem 1000000}),

        %% format the timestamp
        {{Year, Month, Day}, {Hour, Minute, Second}} = UTC_Timestamp,
        Formatted_DateTime = lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Year, Month, Day, Hour, Minute, Second])),
        Formatted_Time     = lists:flatten(io_lib:format("~2.10.0B:~2.10.0B:~2.10.0B", [Hour, Minute, Second])),

        dict:from_list([{'time',             Formatted_Time},
                        {'datetime',         Formatted_DateTime},
                        {'message_severity', "?"},
                        {'message',          binary_to_list(proplists:get_value(event, Message_Elements))}])
      end, folsom_metrics:get_history_values(Metric_Name, 1000)).

