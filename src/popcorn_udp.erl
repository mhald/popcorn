-module(popcorn_udp).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(to_int(Value), list_to_integer(binary_to_list(Value))).

-include("popcorn.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
-record(state, {socket}).

init([]) ->
    {ok, Socket} = gen_udp:open(9125, [binary, {active, once}, {recbuf, 524288}]),

    {ok, #state{socket = Socket}}.

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({udp, Socket, _Host, _Port, Bin}, State) ->
  	io:format("Bin = ~p\n", [Bin]),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};

handle_info(timeout, State) ->
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

