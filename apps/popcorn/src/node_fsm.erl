-module(node_fsm).
-author('marc.e.campbell@gmail.com').
-behavior(gen_fsm).

-include("include/popcorn.hrl").

-export([start_link/0]).

-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-export([
    'LOGGING'/2,
    'LOGGING'/3]).

-record(state, {history_name          :: atom(),
                severity_metric_names :: list(),
                most_recent_version   :: string(),
                node_name             :: binary(),
                role                  :: string()}).

start_link() -> gen_fsm:start_link(?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),

    {ok, 'LOGGING', #state{}}.


'LOGGING'({log_message, Node, Node_Role, Node_Version, Severity, Message}, State) ->
    %% log the message
    ets:insert(State#state.history_name, #log_message{timestamp = ?NOW,
                                                      severity  = Severity,
                                                      message   = Message}),

    %% increment the severity counter for this node
    folsom_metrics:notify({proplists:get_value(Severity, State#state.severity_metric_names), {inc, 1}}),

    %% increment the total event counter
    folsom_metrics:notify({?TOTAL_EVENT_COUNTER, {inc, 1}}),

    %% ensure the metric exists for this hour, severity combination and increment
    Prefix    = <<"_popcorn__">>,
    Hour      = list_to_binary(popcorn_util:hour()),
    SeverityB = list_to_binary(integer_to_list(Severity)),
    Sep       = <<"_">>,

    Node_Severity_History_Counter = binary_to_atom(<<Prefix/binary, Sep/binary, Node/binary, Sep/binary, SeverityB/binary, Sep/binary, Hour/binary>>, latin1),
    Total_Severity_History_Counter = binary_to_atom(<<Prefix/binary, Sep/binary, SeverityB/binary, Sep/binary, Hour/binary>>, latin1),

    case folsom_metrics:metric_exists(Node_Severity_History_Counter) of
        false -> folsom_metrics:new_counter(Node_Severity_History_Counter);
        true  -> ok
    end,

    case folsom_metrics:metric_exists(Total_Severity_History_Counter) of
        false -> folsom_metrics:new_counter(Total_Severity_History_Counter);
        true  -> ok
    end,

    folsom_metrics:notify({Node_Severity_History_Counter, {inc, 1}}),
    folsom_metrics:notify({Total_Severity_History_Counter, {inc, 1}}),

    {next_state, 'LOGGING', State}.

'LOGGING'({set_node_name, Node_Name, Role}, _From, State) ->
    Prefix           = <<"_popcorn__">>,
    History_Name     = binary_to_atom(<<Prefix/binary, Node_Name/binary>>, latin1),

    case ets:info(History_Name) of
        undefined -> ok;
        _         -> ets:delete(History_Name)
    end,

    ets:new(History_Name, [named_table, ordered_set, public, {keypos, #log_message.timestamp}]),

    %% 0 = emergency -> 7 = debug
    Separator_Binary = <<30>>,
    Severity_Metric_Names = lists:map(fun(Level) ->
                                Level_Bin = list_to_binary(integer_to_list(Level)),
                                Severity_Counter_Name = binary_to_atom(<<Prefix/binary, Node_Name/binary, Separator_Binary/binary, Level_Bin/binary>>, latin1),
                                {Level, Severity_Counter_Name}
                              end, lists:seq(0, 7)),

    %% create the metrics
    lists:foreach(fun({_, N}) -> folsom_metrics:new_counter(N) end, Severity_Metric_Names),

    {reply, ok, 'LOGGING', State#state{history_name          = History_Name,
                                       node_name             = Node_Name,
                                       severity_metric_names = Severity_Metric_Names,
                                       role                  = Role}};

'LOGGING'(get_message_counts, _From, State) ->
    Severity_Counts = lists:map(fun({Severity, Metric_Name}) ->
                          {lager_util:num_to_level(Severity), folsom_metrics:get_metric_value(Metric_Name)}
                        end, State#state.severity_metric_names),
    Total_Count     = lists:foldl(fun({_, Count}, Total) -> Total + Count end, 0, Severity_Counts),

    {reply, Severity_Counts ++ [{total, Total_Count}], 'LOGGING', State};
'LOGGING'({severity_count_history, Severity}, _From, State) ->
    Last_24_Hours = popcorn_util:last_24_hours(),

    Prefix    = <<"_popcorn__">>,
    SeverityB = list_to_binary(integer_to_list(Severity)),
    Sep       = <<"_">>,
    Node_Name = State#state.node_name,

		Hours_Ago     = lists:seq(0, 23),
    Metric_Names  = lists:map(fun(Hour) -> HourB = list_to_binary(Hour), binary_to_atom(<<Prefix/binary, Sep/binary, Node_Name/binary, Sep/binary, SeverityB/binary, Sep/binary, HourB/binary>>, latin1) end, popcorn_util:last_24_hours()),
		Time_And_Name = lists:zip(Hours_Ago, Metric_Names),

    Values = lists:map(fun({Hour_Ago, Metric_Name}) ->
                 Value = case folsom_metrics:metric_exists(Metric_Name) of
                             false -> 0;
                      			 true  -> folsom_metrics:get_metric_value(Metric_Name)
                 				 end,
								 [{'hours_ago', 0 - Hour_Ago},
									{'count',     Value}]
               end, Time_And_Name),

    {reply, Values, 'LOGGING', State}.

handle_event(Event, StateName, State)                 -> {stop, {StateName, undefined_event, Event}, State}.
handle_sync_event(Event, _From, StateName, State)     -> {stop, {StateName, undefined_event, Event}, State}.
handle_info(_Info, StateName, State)                  -> {next_state, StateName, State}.
terminate(_Reason, _StateName, State)                 ->
    case ets:info(State#state.history_name) of
        undefined -> ok;
        _         -> ets:delete(State#state.history_name)
    end,
    ok.

code_change(_OldVsn, StateName, StateData, _Extra)    -> {ok, StateName, StateData}.



