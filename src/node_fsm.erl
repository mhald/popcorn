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

-record(state, {metric_name           :: atom(),
                severity_metric_names :: list(),
                most_recent_version   :: string(),
                role                  :: string()}).

start_link() -> gen_fsm:start_link(?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),

    {ok, 'LOGGING', #state{}}.


'LOGGING'({log_message, Node, Node_Role, Node_Version, Severity, Message}, State) ->
    %% log the message
    folsom_metrics:notify({State#state.metric_name, Message}),

    %% increment the severity counter
    folsom_metrics:notify({proplists:get_value(Severity, State#state.severity_metric_names), {inc, 1}}),

    {next_state, 'LOGGING', State}.

'LOGGING'({set_node_name, Node_Name, Role}, _From, State) ->
    Prefix           = <<"_popcorn__">>,
    Metric_Name      = binary_to_atom(<<Prefix/binary, Node_Name/binary>>, latin1),

    case folsom_metrics:metric_exists(Metric_Name) of
        false -> ok;
        true  -> folsom_metrics:delete_metric(Metric_Name)
    end,

    folsom_metrics:new_history(Metric_Name),

    %% 0 = emergency -> 7 = debug
    Separator_Binary = <<30>>,
    Severity_Metric_Names = lists:map(fun(Level) ->
                                Level_Bin = list_to_binary(integer_to_list(Level)),
                                Severity_Counter_Name = binary_to_atom(<<Prefix/binary, Node_Name/binary, Separator_Binary/binary, Level_Bin/binary>>, latin1),
                                {Level, Severity_Counter_Name}
                              end, lists:seq(0, 7)),

    %% create the metrics
    lists:foreach(fun({_, N}) -> folsom_metrics:new_counter(N) end, Severity_Metric_Names),

    {reply, ok, 'LOGGING', State#state{metric_name           = Metric_Name,
                                       severity_metric_names = Severity_Metric_Names,
                                       role                  = Role}};

'LOGGING'(get_message_counts, _From, State) ->
    Severity_Counts = lists:map(fun({Severity, Metric_Name}) ->
                          {lager_util:num_to_level(Severity), folsom_metrics:get_metric_value(Metric_Name)}
                        end, State#state.severity_metric_names),
    Total_Count     = lists:foldl(fun({_, Count}, Total) -> Total + Count end, 0, Severity_Counts),

    {reply, Severity_Counts ++ [{total, Total_Count}], 'LOGGING', State}.

handle_event(Event, StateName, State)                 -> {stop, {StateName, undefined_event, Event}, State}.
handle_sync_event(Event, _From, StateName, State)     -> {stop, {StateName, undefined_event, Event}, State}.
handle_info(_Info, StateName, State)                  -> {next_state, StateName, State}.
terminate(_Reason, _StateName, State)                 -> ok.

code_change(_OldVsn, StateName, StateData, _Extra)    -> {ok, StateName, StateData}.



