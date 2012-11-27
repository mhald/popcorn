-module(http_log_handler).
-author('marc.e.campbell@gmail.com').

-behavior(cowboy_http_handler).

-include("include/popcorn.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([init/3,
         handle/2,
         terminate/2]).


init({_Any, http}, Req, _) -> {ok, Req, undefined_state}.

handle(Req, State) ->
    {Path, Req1}   = cowboy_req:path(Req),
    Path_Parts     = lists:filter(fun(<<>>) -> false; (_) -> true end, binary:split(Path, <<"/">>, [global])),
    {Method, Req2} = cowboy_req:method(Req1),
    handle_path(Method, Path_Parts, Req2, State).

terminate(_Req, _State) -> ok.

handle_path(<<"POST">>, [<<"log">>, <<"stream">>, <<"pause">>], Req, State) ->
    {ok, Vals, _} = cowboy_req:body_qs(Req),
    Stream_Id     = proplists:get_value(<<"stream_id">>, Vals),
    Stream_Pid    = lists:nth(1, ets:select(current_log_streams, ets:fun2ms(fun(#log_stream{stream_id  = SID,
                                                                                            stream_pid = SPID}) when SID =:= Stream_Id -> SPID end))),

    gen_fsm:send_all_state_event(Stream_Pid, toggle_pause),

    %% get the current paused state for the response
    Is_Paused = gen_fsm:sync_send_all_state_event(Stream_Pid, is_paused),
    Response  = {struct, [{"is_paused", Is_Paused}]},

    {ok, Reply}   = cowboy_req:reply(200, [{"Content-Type", "application/json"}], lists:flatten(mochijson:encode(Response)), Req),
    {ok, Reply, State};

handle_path(<<"GET">>, [<<"log">>, <<"stream">>, Stream_Id], Req, State) ->
    Headers      = [{"Content-Type", <<"text/event-stream">>}],
    {ok, Reply}  = cowboy_req:chunked_reply(200, Headers, Req),

    Stream_Pid    = lists:nth(1, ets:select(current_log_streams, ets:fun2ms(fun(#log_stream{stream_id  = SID,
                                                                                            stream_pid = SPID}) when SID =:= Stream_Id -> SPID end))),

    gen_fsm:send_event(Stream_Pid, {set_client_pid, self()}),

    handle_loop(Reply, State);

%% convienence entry points to allow the user to pre-define the filters
handle_path(<<"GET">>, [<<"log">>], Req, State) ->
    case session_handler:is_session_authed_and_valid(Req) of
        false -> Req1 = cowboy_req:set_resp_cookie(<<"popcorn-session-key">>, <<>>, [{path, "/"}], Req),
                 {ok, Reply} = cowboy_req:reply(301, [{"Location", "/login"}], [], Req1),
                 {ok, Reply, State};
        true  -> {Nodes_Param, _}      = cowboy_req:qs_val(<<"nodes">>, Req, <<>>),
                 {Severities_Param, _} = cowboy_req:qs_val(<<"severities">>, Req, <<>>),
                 {Roles_Param, _}      = cowboy_req:qs_val(<<"roles">>, Req, <<>>),

                 Nodes       = string:tokens(binary_to_list(Nodes_Param), ";"),
                 Severities  = string:tokens(binary_to_list(Severities_Param), ";"),
                 Roles       = string:tokens(binary_to_list(Roles_Param), ";"),

                 %% we show everything, unless we have applied a filter.
                 %% so visiting /log will show ALL logs, ALL severities, ALL nodes, ALL roles, etc
                 %% but as soon as we add "severities=debug;info" to the qs, then it's ALL of everything
                 %% except severity, where we now have a filter applies

                 Default_Filters = lists:filter(fun({_, []}) -> false; (_) -> true end,
                                       [{'node_names', Nodes},
                                        {'roles',      Roles},
                                        {'severities', lists:map(fun(Severity_Name) -> popcorn_util:severity_to_number(Severity_Name) end, Severities)}]),

                 %% spawn the stream fsm
                 {ok, Stream_Pid} = supervisor:start_child(stream_sup, []),

                 %% create the stream object
                 Log_Stream = #log_stream{stream_id        = popcorn_util:random_id(),
                                          stream_pid       = Stream_Pid,
                                          client_pid       = undefined,
                                          applied_filters  = Default_Filters,
                                          paused           = false},

                 %% assign to the fsm
                 gen_fsm:send_event(Stream_Pid, {connect, Log_Stream}),

                 Context = dict:from_list([{stream_id,       binary_to_list(Log_Stream#log_stream.stream_id)},
                                          {default_filters, dict:from_list(Default_Filters)}]),

                 TFun        = mustache:compile(view_log),
                 Output      = mustache:render(view_log, TFun, Context),
                 {ok, Reply} = cowboy_req:reply(200, [], Output, Req),
                 {ok, Reply, State}
    end.

handle_loop(Req, State) ->
    receive
        logout -> 
            {ok, Req, State};
        {cowboy_req, resp_sent} ->
            handle_loop(Req, State);
        {new_message, Log_Message} ->
            Params = popcorn_util:format_log_message(Log_Message),
            Json_Event = {struct, Params},
            Event      = lists:flatten(mochijson:encode(Json_Event)),
            ?POPCORN_DEBUG_MSG("sending ~p", [Event]),
            case cowboy_req:chunk(lists:flatten(["data: ", Event, "\n\n"]), Req) of
                ok -> handle_loop(Req, State);
                {error, closed} -> {ok, Req, State}
            end;
        Other ->
            ?POPCORN_DEBUG_MSG("streaming log handler received unknown message: ~p", [Other]),
            Event = ["data: ", "test", "\n\n"],
            ok = cowboy_req:chunk(Event, Req),
            handle_loop(Req, State)
    end.
