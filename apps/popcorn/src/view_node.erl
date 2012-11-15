-module(view_node).
-author('marc.e.campbell@gmail.com').

-include("include/popcorn.hrl").

-export([debug_message_counts/1,
         username/0]).

-spec debug_message_counts(dict()) -> list().
debug_message_counts(Context) ->
    Node_Name = mustache:get(node_name, Context),

    ?POPCORN_DEBUG_MSG("Node Name = ~p\n", [Node_Name]),

    Pid = proplists:get_value(Node_Name, ets:tab2list(current_nodes)),
    Debug_Message_Counts = gen_fsm:sync_send_event(Pid, {severity_count_history, 7}),

    io:format("Debug_Message_Counts = ~p\n", [Debug_Message_Counts]),

    dict:from_list(Debug_Message_Counts).

-spec username() -> string().
username() -> "marc".
