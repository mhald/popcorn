-module(popcorn).

-export([
         start/0, stop/0
        ]).

-define(SERVER, popcorn_server).

%% @spec start() -> ok
%% @doc Start the popcorn server.
start() 			-> [application:start(App) || App <-
										[ranch, cowboy, mustache,
										 protobuffs, inets, mochiweb,
										 webmachine, folsom, popcorn
										 ]].

%% @spec stop() -> ok
%% @doc Stop the popcorn server.
stop() ->
    application:stop(popcorn).

