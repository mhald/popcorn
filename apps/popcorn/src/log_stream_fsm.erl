-module(log_stream_fsm).
-author('marc.e.campbell@gmail.com').
-behavior(gen_fsm).

-include("include/popcorn.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(IDLE_DISCONNECT_TIMER,      5000).

-export([start_link/0]).

-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).


-export([
    'STARTING'/2,
    'STARTING'/3,
    'STREAMING'/2,
    'STREAMING'/3]).

-record(state, {log_stream              :: #log_stream{},
                idle_loops_disconnected :: integer()}).

start_link() -> gen_fsm:start_link(?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),

    gen_fsm:start_timer(?IDLE_DISCONNECT_TIMER, idle_disconnect),

    {ok, 'STARTING', #state{idle_loops_disconnected = 0}}.

'STARTING'({connect, Log_Stream}, State) ->
    %% add to the ets table
    ets:insert(current_log_streams, Log_Stream),

    {next_state, 'STARTING', State#state{log_stream = Log_Stream}};
'STARTING'({set_client_pid, Pid}, State) ->
    Log_Stream  = State#state.log_stream,
    Log_Stream2 = lists:nth(1, ets:select(current_log_streams, ets:fun2ms(fun(Log_Stream3) when Log_Stream3#log_stream.stream_id =:= Log_Stream#log_stream.stream_id -> Log_Stream3 end))),
    Log_Stream3 = Log_Stream2#log_stream{client_pid = Pid},
    ets:insert(current_log_streams, Log_Stream3),

    {next_state, 'STREAMING', State#state{log_stream = Log_Stream3}}.
'STARTING'(Other, _From, State) ->
    {noreply, undefined, 'STARTING', State}.

'STREAMING'({timeout, _From, idle_disconnect}, State) ->
    gen_fsm:start_timer(?IDLE_DISCONNECT_TIMER, idle_disconnect),
    Is_Connected = (State#state.log_stream)#log_stream.client_pid =/= undefined andalso
                   erlang:is_process_alive((State#state.log_stream)#log_stream.client_pid),
    case Is_Connected of
        true ->  {next_state, 'STREAMING', State#state{idle_loops_disconnected = 0}};
        false -> case State#state.idle_loops_disconnected of
                     4 -> {stop, normal, State};
                     O -> {next_state, 'STREAMING', State#state{idle_loops_disconnected = O + 1}}
                 end
    end;
'STREAMING'({update_severities, New_Severities}, State) ->
    Severity_Filter = lists:map(fun(S) -> list_to_integer(S) end, string:tokens(binary_to_list(New_Severities), ",")),
    Log_Stream      = State#state.log_stream,
    Applied_Filters = Log_Stream#log_stream.applied_filters,

    Applied_Filters2 = case proplists:get_value('severities', Applied_Filters) of
                           undefined -> Applied_Filters ++ [{'severities', Severity_Filter}];
                           _         -> proplists:delete('severities', Applied_Filters) ++ [{'severities', Severity_Filter}]
                       end,

    update_filters(Applied_Filters2, Log_Stream#log_stream.stream_id),

    Log_Stream2 = Log_Stream#log_stream{applied_filters = Applied_Filters2},

    {next_state, 'STREAMING', State#state{log_stream = Log_Stream2}};
'STREAMING'(Other, State) ->
    {next_state, 'STREAMING', State}.

'STREAMING'(Other, _From, State) ->
    {noreply, undefined, 'STREAMING', State}.

handle_event({new_message, Log_Message}, State_Name, State) ->
    Log_Stream    = State#state.log_stream,
    Should_Stream = Log_Stream#log_stream.paused =:= false andalso
                    is_filtered_out(Log_Message, Log_Stream#log_stream.applied_filters) =:= false,

    case Should_Stream of
        false -> ok;
        true  -> Log_Stream#log_stream.client_pid ! {new_message, Log_Message}
    end,

    {next_state, State_Name, State};

handle_event(toggle_pause, State_Name, State) ->
    Log_Stream = State#state.log_stream,
    New_Log_Stream = Log_Stream#log_stream{paused = not Log_Stream#log_stream.paused},

    %% TODO, update the ets table with the paused state

    {next_state, State_Name, State#state{log_stream = New_Log_Stream}};

handle_event(Event, StateName, State)                 -> {stop, {StateName, undefined_event, Event}, State}.

handle_sync_event(is_paused, _From, State_Name, State) ->
    Log_Stream = State#state.log_stream,
    {reply, Log_Stream#log_stream.paused, State_Name, State};

handle_sync_event(Event, _From, StateName, State)     -> {stop, {StateName, undefined_event, Event}, State}.
handle_info(_Info, StateName, State)                  -> {next_state, StateName, State}.
terminate(_Reason, _StateName, State)                 ->
    Stream_Id = (State#state.log_stream)#log_stream.stream_id,
    1 = ets:select_delete(current_log_streams, ets:fun2ms(fun(#log_stream{stream_id = SID}) when SID =:= Stream_Id -> true end)),
    ok.
code_change(_OldVsn, StateName, StateData, _Extra)    -> {ok, StateName, StateData}.

update_filters(Applied_Filters, Stream_Id) ->
    Log_Stream = lists:nth(1, ets:select(current_log_streams, ets:fun2ms(fun(Log_Stream2) when Log_Stream2#log_stream.stream_id =:= Stream_Id -> Log_Stream2 end))),
    Log_Stream2 = Log_Stream#log_stream{applied_filters = Applied_Filters},
    ets:insert(current_log_streams, Log_Stream2).

is_filtered_out(Log_Message, Filters) ->
    Severity_Restricted = not lists:member(Log_Message#log_message.severity, proplists:get_value('severities', Filters, [])),
    Severity_Restricted.
