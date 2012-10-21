-module(popcorn_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Children = [
                {popcorn_server, 
                 {popcorn_server, start_link, []},
                 permanent, 5000, worker, [popcorn_server]},

                {popcorn_udp,
                 {popcorn_udp, start_link, []},
                  permanent, 5000, worker, [popcorn_udp]}
               ],
    {ok, { {one_for_one, 10000, 10}, Children} }.

