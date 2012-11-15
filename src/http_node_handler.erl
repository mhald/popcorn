-module(http_node_handler).
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

%% TODO return a 404 if the node isn't found
handle_path(<<"GET">>, [<<"node">>, Node_Name], Req, State) ->
    Context     = dict:from_list([{node_name, Node_Name}]),
    TFun        = mustache:compile(view_node),
    Output      = mustache:render(view_node, TFun, Context),
    {ok, Reply} = cowboy_req:reply(200, [], Output, Req),
    {ok, Reply, State}.



