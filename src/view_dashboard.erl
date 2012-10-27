-module(view_dashboard).
-author('marc.e.campbell@gmail.com').

-include("include/popcorn.hrl").

-export([known_nodes/0]).

-spec known_nodes() -> list().
known_nodes() ->
    lists:map(fun({Node, Pid}) ->
        Message_Counts  = gen_fsm:sync_send_event(Pid, get_message_counts),
        Node_List       = binary_to_list(Node),
        Node_Properties = [{'node_name',          Node_List},
                           {'total_messages',     proplists:get_value(total,     Message_Counts, 0)},
                           {'debug_messages',     proplists:get_value(debug,     Message_Counts, 0)},
                           {'info_messages',      proplists:get_value(info,      Message_Counts, 0)},
                           {'notice_messages',    proplists:get_value(notice,    Message_Counts, 0)},
                           {'warning_messages',   proplists:get_value(warning,   Message_Counts, 0)},
                           {'error_messages',     proplists:get_value(error,     Message_Counts, 0)},
                           {'critical_messages',  proplists:get_value(critical,  Message_Counts, 0)},
                           {'alert_messages',     proplists:get_value(alert,     Message_Counts, 0)},
                           {'emergency_messages', proplists:get_value(emergency, Message_Counts, 0)}],
        dict:from_list(Node_Properties)
      end, ets:tab2list(current_nodes)).





