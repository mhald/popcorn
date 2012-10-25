-module(http_static_handler).
-author('marc.e.campbell@gmail.com').

-behavior(cowboy_http_handler).

-include("include/popcorn.hrl").

-export([init/3,
         handle/2,
         terminate/2]).

init({tcp, http}, Req, [])        -> {ok, Req, undefined_state}.

handle(Req, State) ->
    {Requested_File, _} = cowboy_req:path(Req),
    send(Req, Requested_File, State).

send(Req, Path_Bin, State) ->
    Path = code:priv_dir(popcorn) ++ binary_to_list(Path_Bin),
    Content_Type = guess_content_type(list_to_binary(Path)),

    case file_contents(Path) of
        {ok, Body} ->
            Headers = [{<<"Content-Type">>, Content_Type}],
            {ok, Reply} = cowboy_req:reply(200, Headers, Body, Req),
            {ok, Reply, State};
        _ ->
            {ok, Reply} = cowboy_req:reply(404, [], <<>>, Req),
            {ok, Reply, State}
    end.

terminate(_Req, _State) ->
    ok.

guess_content_type(Filename) ->
    case filename:extension(Filename) of
        <<".js">>  -> <<"application/javascript">>;
        <<".css">> -> <<"text/css">>;
        <<".png">> -> <<"image/png">>;
        _          -> <<"application/octet-stream">>
    end.

file_contents(Path) -> file:read_file(Path).

