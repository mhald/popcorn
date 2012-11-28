-module(http_catchall_handler).
-author('marc.e.campbell@gmail.com').

-behavior(cowboy_http_handler).

-include("include/popcorn.hrl").

-export([init/3,
         handle/2,
         terminate/2]).

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
            Output = mustache:render(view_login),
            {ok, Reply} = cowboy_req:reply(200, [], Output, Req),
            {ok, Reply, State};

        {{<<"POST">>, _}, {<<"/api/v1/login">>, _}} ->
            ?POPCORN_DEBUG_MSG("http api login"),
            {ok, Post_Vals, Req2} = cowboy_req:body_qs(Req),
            Username = proplists:get_value(<<"username">>, Post_Vals),
            Password = proplists:get_value(<<"password">>, Post_Vals),

            case session_handler:try_start_authed_session("", Username, Password) of
                {error, Message}  -> {ok, Reply} = cowboy_req:reply(401, [], Message, Req),
                                     {ok, Reply, State};
                {ok, Session_Key} -> Req1 = cowboy_req:set_resp_cookie(<<"popcorn-session-key">>, Session_Key, [{path, "/"}], Req),
                                     {ok, Reply} = cowboy_req:reply(200, [], <<>>, Req1),
                                     {ok, Reply, State}
            end;

        {{<<"GET">>, _}, {<<"/">>, _}} ->
            ?POPCORN_DEBUG_MSG("http request for dashboard"),
            case session_handler:is_session_authed_and_valid(Req) of
                false -> Req1 = cowboy_req:set_resp_cookie(<<"popcorn-session-key">>, <<>>, [{path, "/"}], Req),
                         {ok, Reply} = cowboy_req:reply(301, [{"Location", "/login"}], [], Req1),
                         {ok, Reply, State};
                true  -> Output = mustache:render(view_dashboard),
                         {ok, Reply} = cowboy_req:reply(200, [], Output, Req),
                         {ok, Reply, State}
            end;

        {{Method, _}, {Path, _}} ->
            ?POPCORN_DEBUG_MSG("not implemented in catchall http request: ~p, ~p", [Method, Path]),
            {ok, Reply} = cowboy_req:reply(405, [], <<>>, Req),
            {ok, Reply, State}
    end.

terminate(_Req, _State) -> ok.
