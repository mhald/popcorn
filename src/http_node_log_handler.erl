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

handle_path(<<"GET">>, [<<"node">>, Node_Name, <<"log">>], Req, State) ->
    handle_path(<<"GET">>, [<<"node">>, Node_Name, <<"log">>, <<"all">>], Req, State);
handle_path(<<"GET">>, [<<"node">>, Node_Name, <<"log">>, Severity], Req, State) ->
    Prefix           = <<"_popcorn__">>,
    Metric_Name      = binary_to_atom(<<Prefix/binary, Node_Name/binary>>, latin1),

    ?POPCORN_DEBUG_MSG("getting logs for node ~p (~p), severity = ~p", [Node_Name, Metric_Name, Severity]),

    case folsom_metrics:get_history_values(Metric_Name, 1000) of
        {error, bad_module} -> session_handler:return_404(Req, State);
        Log_Messages        -> Context = dict:from_list([{node_name, Node_Name},
                                                         {severity,  Severity}]),
                               TFun = mustache:compile(view_node_log),
                               Output = mustache:render(view_node_log, TFun, Context),
                               {ok, Reply} = cowboy_req:reply(200, [], Output, Req),
                               {ok, Reply, State}
    end.


