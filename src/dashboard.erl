-module(dashboard).
-author('marc.e.campbell@gmail.com').

-include("include/popcorn.hrl").

-export([render/0]).

-spec render() -> string().
render() ->
    Nodes_With_Counts = lists:map(fun({Node, Pid}) ->
        Message_Counts = gen_fsm:sync_send_event(Pid, get_message_counts),
        [{"name",               binary_to_list(Node)},
         {"total_messages",     proplists:get_value(total,     Message_Counts, 0)},
         {"debug_messages",     proplists:get_value(debug,     Message_Counts, 0)},
         {"info_messages",      proplists:get_value(info,      Message_Counts, 0)},
         {"notice_messages",    proplists:get_value(notice,    Message_Counts, 0)},
         {"error_messages",     proplists:get_value(error,     Message_Counts, 0)},
         {"critical_messages",  proplists:get_value(critical,  Message_Counts, 0)},
         {"alert_messages",     proplists:get_value(alert,     Message_Counts, 0)},
         {"emergency_messages", proplists:get_value(emergency, Message_Counts, 0)}]
      end, ets:tab2list(current_nodes)),

    io:format("Nodes_With_Counts = ~p\n", [Nodes_With_Counts]),

    Context = dict:from_list(Nodes_With_Counts),
    mustache:render(popcorn, ?MUSTACHE("dashboard.mustache"), Context).

