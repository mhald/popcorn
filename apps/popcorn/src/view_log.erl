-module(view_log).
-author('marc.e.campbell@gmail.com').

-include_lib("stdlib/include/ms_transform.hrl").
-include("include/popcorn.hrl").

-export([known_roles/1,
         known_nodes/1,
         known_severities/1,
         username/0,
         streaming_url/1]).

-spec known_roles(dict()) -> list().
known_roles(Context) ->
    Role_Names        = lists:map(fun({Name, _}) -> Name end, ets:tab2list(current_roles)),
    Unique_Role_Names = sets:to_list(sets:from_list(Role_Names)),
    lists:map(fun(Role_Name) ->
        Params = [{'role',         binary_to_list(Role_Name)},
                  {'filter_state', ""}],
        dict:from_list(Params)
      end, Unique_Role_Names).

-spec known_nodes(dict()) -> list().
known_nodes(Context) ->
    Default_Filters = dict:to_list(proplists:get_value(default_filters, dict:to_list(Context))),
    Filtered_On_Nodes = proplists:get_value('node_names', Default_Filters, []),

    Node_Names = lists:map(fun({Name, _}) -> Name end, ets:tab2list(current_nodes)),
    lists:map(fun(Node_Name) ->
        Filter_State = case lists:member(Node_Name, Filtered_On_Nodes) of
                           false -> "";
                           true  -> " checked"
                       end,
        Params = [{'name',         binary_to_list(Node_Name)},
                  {'filter_state', Filter_State}],
        dict:from_list(Params)
      end, Node_Names).

-spec known_severities(dict()) -> list().
known_severities(Context) ->
    Default_Filters = dict:to_list(proplists:get_value(default_filters, dict:to_list(Context))),
    Filtered_On_Severities = proplists:get_value('severities', Default_Filters, []),

    lists:map(fun(Severity_Number) ->
        Filter_State = case lists:member(Severity_Number, Filtered_On_Severities) of
                           false -> "";
                           true  -> " checked"
                       end,
        Params = [{'label',        binary_to_list(popcorn_util:number_to_severity(Severity_Number))},
                  {'filter_state', Filter_State}],
        dict:from_list(Params)
      end, lists:reverse(popcorn_util:all_severity_numbers())).

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

