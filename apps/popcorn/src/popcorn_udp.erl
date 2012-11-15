-module(popcorn_udp).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(to_int(Value), list_to_integer(binary_to_list(Value))).

-include("include/popcorn.hrl").

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

init(Params) ->
    {ok, Udp_Listen_Port} = application:get_env(popcorn, udp_listen_port),
    {ok, Socket} = gen_udp:open(Udp_Listen_Port, [binary, {active, once}, {recbuf, 524288}]),

    {ok, #state{socket = Socket}}.

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({udp, Socket, _Host, _Port, Bin}, State) ->
    {Node, Node_Role, Node_Version, Severity, Message} = decode_protobuffs_message(Bin),

    %% create the node fsm, if necessary
    case ets:select_count(current_nodes, [{{'$1', '$2'}, [{'=:=', '$1', Node}], [true]}]) of
        0 -> {ok, Pid} = supervisor:start_child(node_sup, []),
             ok = gen_fsm:sync_send_event(Pid, {set_node_name, Node, Node_Role}),
             ets:insert(current_nodes, {Node, Pid});
        _ -> ok
    end,

    %% let the fsm create the log
    case ets:lookup(current_nodes, Node) of
        []                 -> ?POPCORN_WARN_MSG("unable to find fsm for node ~p", [Node]);
        [{_, Running_Pid}] -> gen_fsm:send_event(Running_Pid, {log_message, Node, Node_Role, Node_Version, Severity, Message})
    end,

    %%Tags = get_tags(binary_to_list(Message)),

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
    {{1, Node},     Rest1}     = protobuffs:decode(Encoded_Message, bytes),
    {{2, Node_Role}, Rest2}    = protobuffs:decode(Rest1,           bytes),
    {{3, Node_Version}, Rest3} = protobuffs:decode(Rest2,           bytes),
    {{4, Severity}, Rest4}     = protobuffs:decode(Rest3,           bytes),
    {{5, Message},  <<>>}      = protobuffs:decode(Rest4,           bytes),

    {Node, Node_Role, Node_Version, Severity, Message}.

