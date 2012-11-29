-module(view_log).
-author('marc.e.campbell@gmail.com').

-include_lib("stdlib/include/ms_transform.hrl").
-include("include/popcorn.hrl").

-export([head_includes/0,
         known_roles/1,
         known_nodes/1,
         known_severities/1,
         applied_filters/1,
         username/0,
         streaming_url/1]).

-spec head_includes() -> list().
head_includes() -> popcorn_util:head_includes().

-spec known_roles(dict()) -> list().
known_roles(_) ->
    Role_Names        = lists:map(fun({Name, _}) -> Name end, ets:tab2list(current_roles)),
    Unique_Role_Names = sets:to_list(sets:from_list(Role_Names)),
    lists:map(fun(Role_Name) ->
        Params = [{'role',         binary_to_list(Role_Name)}],
        dict:from_list(Params)
      end, Unique_Role_Names).

-spec known_nodes(dict()) -> list().
known_nodes(_) ->
    Node_Names = lists:map(fun({Name, _}) -> Name end, ets:tab2list(current_nodes)),
    lists:map(fun(Node_Name) ->
        Params = [{'name',         binary_to_list(Node_Name)}],
        dict:from_list(Params)
      end, Node_Names).

-spec known_severities(dict()) -> list().
known_severities(_) ->
    lists:map(fun(Severity_Number) ->
        Params = [{'label',        binary_to_list(popcorn_util:number_to_severity(Severity_Number))},
                  {'severity_num', integer_to_list(Severity_Number)}],
        dict:from_list(Params)
      end, lists:reverse(popcorn_util:all_severity_numbers())).

-spec applied_filters(dict()) -> string().
applied_filters(Context) ->
    Default_Filters = dict:to_list(proplists:get_value(default_filters, dict:to_list(Context))),
    Json = {struct, lists:map(fun({Name, Value}) ->
                        {atom_to_list(Name), {array, Value}}
                      end, Default_Filters)},
    lists:flatten(mochijson:encode(Json)).

-spec username() -> string().
username() -> "marc".

-spec streaming_url(dict()) -> string().
streaming_url(Context) ->
    {ok, Http_Listen_Host} = get_opt_env(popcorn, http_listen_host, "localhost"),
    {ok, Http_Listen_Port} = get_opt_env(popcorn, http_listen_port, 9125),

    "http://" ++
        Http_Listen_Host ++
        ":" ++
        integer_to_list(Http_Listen_Port) ++
        "/log/stream/" ++ mustache:get(stream_id, Context).

get_opt_env(Mod, Var, Default) ->
    case application:get_env(Mod, Var) of
        {ok, Val} -> {ok, Val};
        _         -> {ok, Default}
    end.

