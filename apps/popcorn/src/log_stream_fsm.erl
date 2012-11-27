-module(log_stream_fsm).
-author('marc.e.campbell@gmail.com').
-behavior(gen_fsm).

-include("include/popcorn.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(IDLE_DISCONNECT_TIMER,      60000).

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
    'STREAMING'/3,
    'PAUSED'/2,
    'PAUSED'/3]).

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

    {next_state, 'STREAMING', State#state{log_stream = Log_Stream}}.

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
'STREAMING'(Other, State) ->
    {next_state, 'STREAMING', State}.

'STREAMING'(Other, _From, State) ->
    {noreply, undefined, 'STREAMING', State}.

'PAUSED'(Other, State) ->
    {next_state, 'PAUSED', State}.

'PAUSED'(Other, _From, State) ->
    {noreply, undefined, 'PAUSED', State}.

handle_event({new_message, Log_Message}, State_Name, State) ->
    Log_Stream    = State#state.log_stream,
    Should_Stream = Log_Stream#log_stream.paused =:= false andalso
                    is_filtered_out(Log_Message, Log_Stream#log_stream.applied_filters) =:= false,

    case Should_Stream of
        false -> ok;
        true  -> Log_Stream#log_stream.client_pid ! {new_message, Log_Message}
    end,

    {next_state, State_Name, State};

handle_event(Event, StateName, State)                 -> {stop, {StateName, undefined_event, Event}, State}.
handle_sync_event(Event, _From, StateName, State)     -> {stop, {StateName, undefined_event, Event}, State}.
handle_info(_Info, StateName, State)                  -> {next_state, StateName, State}.
terminate(_Reason, _StateName, State)                 ->
    Stream_Id = (State#state.log_stream)#log_stream.stream_id,
    1 = ets:select_delete(current_log_streams, ets:fun2ms(fun(#log_stream{stream_id = SID}) when SID =:= Stream_Id -> true end)),
    ok.
code_change(_OldVsn, StateName, StateData, _Extra)    -> {ok, StateName, StateData}.


is_filtered_out(Log_Message, Filters) -> false.
