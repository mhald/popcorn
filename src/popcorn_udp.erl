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
    {Node, Severity, Message} = decode_protobuffs_message(Bin),

    Tags = get_tags(binary_to_list(Message)),

    io:format("Node = ~p, Message = ~p\n, Tags = ~p\n", [Node, Message, Tags]),

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


get_tags(Message) ->
    Tags_List = lists:filter(fun(Word) ->
          string:substr(Word, 1, 1) =:= "#"
      end, string:tokens(Message, " ")),
    Cleaned_Tags = lists:map(fun(Word) ->
          string:substr(Word, 2, length(Word) - 1)
      end, Tags_List),
    string:join(Cleaned_Tags, ",").

decode_protobuffs_message(Encoded_Message) ->
    {{1, Node},     Rest1} = protobuffs:decode(Encoded_Message, bytes),
    {{2, Severity}, Rest2} = protobuffs:decode(Rest1,           bytes),
    {{3, Message},  <<>>}  = protobuffs:decode(Rest2,           bytes),

    {Node, Severity, Message}.



