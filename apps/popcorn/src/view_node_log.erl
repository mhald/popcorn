-module(view_node_log).
-author('marc.e.campbell@gmail.com').

-include_lib("stdlib/include/ms_transform.hrl").
-include("include/popcorn.hrl").

-export([log_messages/1,
         username/0,
         current_node_name/1,
         streaming_url/1]).

-spec log_messages(dict()) -> list().
log_messages(Context) ->
    Node_Name = mustache:get(node_name, Context),
    Severity  = mustache:get(severity, Context),

    ?POPCORN_DEBUG_MSG("Node Name = ~p\n Severity = ~p", [Node_Name, Severity]),

    Prefix           = <<"_popcorn__">>,
    Table_Name       = binary_to_atom(<<Prefix/binary, Node_Name/binary>>, latin1),
    Severity_Index   = popcorn_util:severity_to_number(Severity),

    case ets:info(Table_Name) of
        undefined -> [];
        _         -> Log_Messages = case Severity_Index of
                         -1 -> ets:select_reverse(Table_Name, ets:fun2ms(fun(N) -> N end));
                         _  -> ets:select_reverse(Table_Name, ets:fun2ms(fun(N = #log_message{severity = S}) when S =:= Severity_Index -> N end))
                     end,

                     lists:map(fun(Log_Message) ->
                         UTC_Timestamp = calendar:now_to_universal_time({Log_Message#log_message.timestamp div 1000000000000, 
                                                                         Log_Message#log_message.timestamp div 1000000 rem 1000000,
                                                                         Log_Message#log_message.timestamp rem 1000000}),
                         {{Year, Month, Day}, {Hour, Minute, Second}} = UTC_Timestamp,
                         Formatted_DateTime = lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Year, Month, Day, Hour, Minute, Second])),
                         Formatted_Time     = lists:flatten(io_lib:format("~2.10.0B:~2.10.0B:~2.10.0B", [Hour, Minute, Second])),

                         dict:from_list([{'time',             Formatted_Time},
                                         {'datetime',         Formatted_DateTime},
                                         {'message_severity', binary_to_list(popcorn_util:number_to_severity(Log_Message#log_message.severity))},
                                         {'message',          binary_to_list(Log_Message#log_message.message)}])
                       end, Log_Messages)
    end.

-spec username() -> string().
username() -> "marc".

-spec current_node_name(dict()) -> string().
current_node_name(Context) -> binary_to_list(mustache:get(node_name, Context)).

-spec streaming_url(dict()) -> string().
streaming_url(Context) ->
    Node_Name = mustache:get(node_name, Context),
    Severity  = mustache:get(severity,  Context),
    {ok, Http_Listen_Host} = get_opt_env(popcorn, http_listen_host, "localhost"),
    {ok, Http_Listen_Port} = get_opt_env(popcorn, http_listen_port, 9125),

    "http://" ++ Http_Listen_Host ++ ":" ++ integer_to_list(Http_Listen_Port) ++
    "/node/" ++ 
    binary_to_list(Node_Name) ++ 
    "/log/" ++
    case Severity of
        <<"any">> -> "";
        _         -> binary_to_list(Severity) ++ "/"
    end ++
    "stream".

get_opt_env(Mod, Var, Default) ->
    case application:get_env(Mod, Var) of
        {ok, Val} -> {ok, Val};
        _         -> {ok, Default}
    end.
