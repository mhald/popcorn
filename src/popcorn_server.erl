-module(popcorn_server).
-behaviour(gen_server).

-export([start_link/0]).

%-export([key2str/1,flush/0]). %% export for debugging 

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%

init([]) ->
	{ok, #state{}}.

handle_cast(_, State) 	    -> {noreply, State}.

handle_call(_,_,State)      -> {reply, ok, State}.

handle_info(_Msg, State)    -> {noreply, State}.

code_change(_, _, State)    -> {noreply, State}.

terminate(_, _)             -> ok.


