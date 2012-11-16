-module(view_node).
-author('marc.e.campbell@gmail.com').

-include("include/popcorn.hrl").

-export([debug_event_counts/1,
				 info_event_counts/1,
			 	 notice_event_counts/1,
			   warn_event_counts/1,
				 error_event_counts/1,
				 critical_event_counts/1,
				 alert_event_counts/1,
				 emergency_event_counts/1,
         username/0,
         current_node_name/1]).

-spec debug_event_counts(dict()) -> list().
debug_event_counts(Context) -> event_counts(mustache:get(node_name, Context), 7).

-spec info_event_counts(dict()) -> list().
info_event_counts(Context) -> event_counts(mustache:get(node_name, Context), 6).

-spec notice_event_counts(dict()) -> list().
notice_event_counts(Context) -> event_counts(mustache:get(node_name, Context), 5).

-spec warn_event_counts(dict()) -> list().
warn_event_counts(Context) -> event_counts(mustache:get(node_name, Context), 4).

-spec error_event_counts(dict()) -> list().
error_event_counts(Context) -> event_counts(mustache:get(node_name, Context), 3).

-spec critical_event_counts(dict()) -> list().
critical_event_counts(Context) -> event_counts(mustache:get(node_name, Context), 2).

-spec alert_event_counts(dict()) -> list().
alert_event_counts(Context) -> event_counts(mustache:get(node_name, Context), 1).

-spec emergency_event_counts(dict()) -> list().
emergency_event_counts(Context) -> event_counts(mustache:get(node_name, Context), 0).
		

-spec event_counts(atom(), integer()) -> list().
event_counts(Node_Name, Severity) ->
		Pid = proplists:get_value(Node_Name, ets:tab2list(current_nodes)),
    Message_Counts = gen_fsm:sync_send_event(Pid, {severity_count_history, Severity}),
	  lists:map(fun(I) -> dict:from_list(I) end, lists:reverse(Message_Counts)).	

-spec username() -> string().
username() -> "marc".

-spec current_node_name(dict()) -> string().
current_node_name(Context) -> binary_to_list(mustache:get(node_name, Context)).
