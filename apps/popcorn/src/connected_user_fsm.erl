-module(connected_user_fsm).
-author('marc.e.campbell@gmail.com').
-behavior(gen_fsm).

-include("include/popcorn.hrl").

-export([start_link/0,
         try_auth_visit/4]).

-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-export([
    'ANONYMOUS'/2,
    'ANONYMOUS'/3,
    'AUTHENTICATED'/2,
    'AUTHENTICATED'/3]).

-record(state, {session_key :: binary(),
                ip_address  :: string(),
                username    :: string()}).

start_link() -> gen_fsm:start_link(?MODULE, [], []).
try_auth_visit(Pid, IP_Address, Username, Password) -> gen_fsm:sync_send_event(Pid, {try_auth_visit, IP_Address, Username, Password}).

init([]) ->
    process_flag(trap_exit, true),

    {ok, 'ANONYMOUS', #state{}}.

'ANONYMOUS'(Other, State) ->
    ?POPCORN_WARN_MSG("unhandled message received in anonymous state: ~p", [Other]),
    {next_state, 'ANONYMOUS', State}.

'ANONYMOUS'({try_auth_visit, IP_Address, Username, Password}, _From, State) ->
    {Auth_Enabled, User_Database} = get_auth_config(),

    case Auth_Enabled of
        false -> {reply, true, 'AUTHENTICATED', State#state{session_key = generate_session_key(),
                                                            ip_address  = IP_Address,
                                                            username    = Username}};
        true  -> case proplists:get_value(Username, User_Database) of
                     Password -> {reply, true, 'AUTHENTICATED', State#state{session_key = generate_session_key(),
                                                                            ip_address  = IP_Address,
                                                                            username    = Username}};
                     _        -> {reply, false, 'ANONYMOUS', State}
                 end
    end.

'AUTHENTICATED'(Other, State) ->
    ?POPCORN_WARN_MSG("unhandled message received in authenticated state: ~p", [Other]),
    {next_state, 'AUTHENTICATED', State}.

'AUTHENTICATED'(get_session_key, _From, State) ->
    {reply, State#state.session_key, 'AUTHENTICATED', State};
'AUTHENTICATED'(Other, _From, State) ->
    ?POPCORN_WARN_MSG("unhandled synchronous message in authenticated state: ~p", [Other]),
    {reply, error, 'AUTHENTICATED', State}.

handle_event(destroy, State_Name, State) ->
    ets:delete(current_connected_users, State#state.session_key),
    {stop, normal, State};

handle_event(Event, StateName, State)                 -> {stop, {StateName, undefined_event, Event}, State}.
handle_sync_event(Event, _From, StateName, State)     -> {stop, {StateName, undefined_event, Event}, State}.
handle_info(_Info, StateName, State)                  -> {next_state, StateName, State}.
terminate(_Reason, _StateName, State)                 -> ok.
code_change(_OldVsn, StateName, StateData, _Extra)    -> {ok, StateName, StateData}.


-spec get_auth_config() -> {boolean(), list()}.
get_auth_config() ->
    case application:get_env(popcorn, http_auth_enabled) of
        {ok, false} -> {false, [{"_", "_"}]};
        {ok, true}  -> case application:get_env(popcorn, http_auth_db_type) of
                           {ok, config} -> {ok, User_Database} = application:get_env(popcorn, http_auth_users),
                                           {true, User_Database};
                           {ok, Other}  -> ?POPCORN_WARN_MSG("unknown auth type: ~p", [Other]),
                                           {false, [{"_", "_"}]}
                 end
    end.

-spec generate_session_key() -> string().
generate_session_key() ->
    Length = 64,
    AllowedChars = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
    random:seed(now()),
    New_Key = lists:foldl(fun(_, Acc) ->
                  [lists:nth(random:uniform(length(AllowedChars)), AllowedChars)] ++ Acc
                end, [], lists:seq(1, Length)),
    list_to_binary(New_Key).

