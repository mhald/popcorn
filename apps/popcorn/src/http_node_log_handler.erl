-module(http_node_log_handler).
-author('marc.e.campbell@gmail.com').

-behavior(cowboy_http_handler).

-include("include/popcorn.hrl").

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

handle_path(<<"GET">>, [<<"node">>, Node_Name, <<"log">>, <<"stream">>], Req, State) ->
    handle_path(<<"GET">>, [<<"node">>, Node_Name, <<"log">>, <<"all">>, <<"stream">>], Req, State);
handle_path(<<"GET">>, [<<"node">>, Node_Name, <<"log">>, Severity, <<"stream">>], Req, State) ->
    ?POPCORN_DEBUG_MSG("SSE Request to stream logs for ~p, Severity: ~p", [Node_Name, Severity]),

    %% store this pid in the ets table so the node can notify when events come in
    ets:insert(current_node_streams, {Node_Name, self()}),

    Headers     = [{"Content-Type", <<"text/event-stream">>}],
    {ok, Reply}  = cowboy_req:chunked_reply(200, Headers, Req),
    handle_loop(Reply, State);

handle_path(<<"GET">>, [<<"node">>, Node_Name, <<"log">>], Req, State) ->
    handle_path(<<"GET">>, [<<"node">>, Node_Name, <<"log">>, <<"all">>], Req, State);
handle_path(<<"GET">>, [<<"node">>, Node_Name, <<"log">>, Severity], Req, State) ->
    Prefix           = <<"_popcorn__">>,
    Table_Name       = binary_to_atom(<<Prefix/binary, Node_Name/binary>>, latin1),

    ?POPCORN_DEBUG_MSG("getting logs for node ~p (~p), severity = ~p", [Node_Name, Table_Name, Severity]),

    case ets:info(Table_Name) of
        undefined -> session_handler:return_404(Req, State);
        _         -> Context = dict:from_list([{node_name, Node_Name},
                                               {severity,  Severity}]),

                     TFun        = mustache:compile(view_node_log),
                     Output      = mustache:render(view_node_log, TFun, Context),
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
            Json_Event = {struct, [{"message", Log_Message#log_message.message},
                                   {"timestamp", Log_Message#log_message.timestamp},
                                   {"severity", popcorn_util:number_to_severity(Log_Message#log_message.severity)}]},
            Event      = lists:flatten(mochijson:encode(Json_Event)),
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
