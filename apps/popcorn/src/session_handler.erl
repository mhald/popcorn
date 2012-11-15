-module(session_handler).
-author('marc.e.campbell@gmail.com').

-include("include/popcorn.hrl").

-export([try_start_authed_session/3,
         is_session_authed_and_valid/1,
         return_404/2]).

-spec try_start_authed_session(string(), string(), string()) -> {error, string()} | {ok, any()}.
-spec is_session_authed_and_valid(any()) -> true | false.
-spec return_404(any(), any()) -> any().

try_start_authed_session(IP_Address, Username, Password) ->
    {ok, Pid} = supervisor:start_child(connected_user_sup, []),
    case connected_user_fsm:try_auth_visit(Pid, IP_Address, Username, Password) of
        false -> gen_fsm:send_all_state_event(Pid, destroy),
                 {error, "Invalid Login"};
        true  -> Session_Key = gen_fsm:sync_send_event(Pid, get_session_key),
                 ets:insert(current_connected_users, {Session_Key, Pid}),
                 {ok, Session_Key}
    end.

is_session_authed_and_valid(Req) ->
    {Session_Key, _} = cowboy_req:cookie(<<"popcorn-session-key">>, Req),
    case Session_Key of
        undefined -> false;
        _         -> io:format("Session_Key = ~p\n", [Session_Key]),
                     case ets:lookup(current_connected_users, Session_Key) of
                         [] -> false;
                         _  -> true
                     end
    end.

return_404(Req, State) ->
    {ok, Reply} = cowboy_req:reply(404, [], <<>>, Req),
    {ok, Reply, State}.
