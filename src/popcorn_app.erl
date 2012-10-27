-module(popcorn_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, init/1]).


-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
stop(_State) -> ok.

init([]) ->
    io:format("CWD: ~p\n", [filename:absname("")]),

    io:format("Creating ets tables..."),
    ets:new(current_connected_users, [named_table, set, public]),
    ets:new(current_nodes, [named_table, set, public]),
    io:format(" done!\n"),

    io:format("Starting http listener..."),
    {ok, Http_Listen_Port} = application:get_env(popcorn, http_listen_port),
    Http_Dispatch = [{'_', [
                            {[<<"js">>, '...'],     http_static_handler, []},
                            {[<<"css">>, '...'],    http_static_handler, []},
                            {[<<"images">>, '...'], http_static_handler, []},
                            {[<<"node">>, '_', <<"log">>, '...'],   http_node_log_handler, []},
                            {'_',                   http_catchall_handler, []}
                           ]}],

    cowboy:start_http(http_handler, 100, [{port, Http_Listen_Port}], [{dispatch, Http_Dispatch}]),
    io:format(" done!\n"),


    Children = [
                  {popcorn_server, {popcorn_server, start_link, []}, permanent, 5000, worker, [popcorn_server]},
                  {popcorn_udp,    {popcorn_udp,    start_link, []}, permanent, 5000, worker, [popcorn_udp]},

                  {connected_user_sup, {supervisor, start_link, [{local, connected_user_sup}, ?MODULE, [connected_user_fsm]]}, permanent, infinity, supervisor, []},
                  {node_sup,           {supervisor, start_link, [{local, node_sup},           ?MODULE, [node_fsm]]},           permanent, infinity, supervisor, []}
               ],

    {ok, { {one_for_one, 10000, 10}, Children} };

init([Module]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              {   undefined,
                  {Module,start_link,[]},
                  temporary,
                  2000,
                  worker,
                  []
              }
            ]
        }
    }.




