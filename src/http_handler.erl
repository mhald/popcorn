-module(http_handler).
-author('marc.e.campbell@gmail.com').

-behavior(cowboy_http_handler).

-include("include/popcorn.hrl").

-export([init/3,
         handle/2,
         terminate/2]).

-spec try_start_authed_session(string(), string(), string()) -> {error, string()} | {ok, any()}.
-spec is_session_authed_and_valid(any()) -> true | false.

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

init({_Any, http}, Req, _) -> {ok, Req, undefined_state}.

handle(Req, State) ->
    case {cowboy_req:method(Req), cowboy_req:path(Req)} of
        {{<<"GET">>, _}, {<<"/logout">>, _}} ->
            ?POPCORN_DEBUG_MSG("http request for logout page"),
            Output = mustache:render(popcorn, ?MUSTACHE("logout.mustache"), dict:from_list([])),
            Req1 = cowboy_req:set_resp_cookie(<<"popcorn-session-key">>, <<>>, [{path, "/"}], Req),
            {ok, Reply} = cowboy_req:reply(200, [], Output, Req1),
            {ok, Reply, State};

        {{<<"GET">>, _}, {<<"/login">>, _}} ->
            ?POPCORN_DEBUG_MSG("http request for login page"),
            Output = mustache:render(popcorn, ?MUSTACHE("login.mustache"), dict:from_list([])),
            {ok, Reply} = cowboy_req:reply(200, [], Output, Req),
            {ok, Reply, State};

        {{<<"POST">>, _}, {<<"/api/v1/login">>, _}} ->
            ?POPCORN_DEBUG_MSG("http api login"),
            {ok, Post_Vals, Req2} = cowboy_req:body_qs(Req),
            Username = proplists:get_value(<<"username">>, Post_Vals),
            Password = proplists:get_value(<<"password">>, Post_Vals),

            case try_start_authed_session("", Username, Password) of
                {error, Message}  -> {ok, Reply} = cowboy_req:reply(401, [], Message, Req),
                                     {ok, Reply, State};
                {ok, Session_Key} -> Req1 = cowboy_req:set_resp_cookie(<<"popcorn-session-key">>, Session_Key, [{path, "/"}], Req),
                                     {ok, Reply} = cowboy_req:reply(200, [], <<>>, Req1),
                                     {ok, Reply, State}
            end;

        {{<<"GET">>, _}, {<<"/">>, _}} ->
            ?POPCORN_DEBUG_MSG("http request for dashboard"),
            case is_session_authed_and_valid(Req) of
                false -> Req1 = cowboy_req:set_resp_cookie(<<"popcorn-session-key">>, <<>>, [{path, "/"}], Req),
                         {ok, Reply}  = cowboy_req:reply(301, [{"Location", "/login"}], [], Req1),
                         {ok, Reply, State};
                true  -> Output = dashboard:render(),
                         {ok, Reply}  = cowboy_req:reply(200, [], Output, Req),
                         {ok, Reply, State}
            end;

        {{Method, _}, {Path, _}} ->
            ?POPCORN_DEBUG_MSG("not implemented http request: ~p, ~p", [Method, Path]),
            {ok, Reply} = cowboy_req:reply(405, [], <<>>, Req),
            {ok, Reply, State}
    end.

terminate(_Req, _State) -> ok.
