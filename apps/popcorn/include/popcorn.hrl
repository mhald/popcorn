
-define(MUSTACHE(Filename),          code:priv_dir(popcorn) ++ "/templates/" ++ Filename).

-define(TOTAL_EVENT_COUNTER,         binary_to_atom(<<"__popcorn__total_events">>, latin1)).
-define(PERCENT(Value),              round(Value * 100 * math:pow(10, 2)) / math:pow(10, 2)).
-define(NOW,                         folsom_utils:now_epoch_micro()).
-define(POPCORN_DEBUG_MSG(Msg),      io:format("~s\n", [Msg]), lager:debug(Msg)).
-define(POPCORN_INFO_MSG(Msg),       io:format("~s\n", [Msg]), lager:info(Msg)).
-define(POPCORN_NOTICE_MSG(Msg),     io:format("~s\n", [Msg]), lager:notice(Msg)).
-define(POPCORN_WARN_MSG(Msg),       io:format("~s\n", [Msg]), lager:warning(Msg)).
-define(POPCORN_ERROR_MSG(Msg),      io:format("~s\n", [Msg]), lager:error(Msg)).
-define(POPCORN_CRITICAL_MSG(Msg),   io:format("~s\n", [Msg]), lager:critical(Msg)).
-define(POPCORN_ALERT_MSG(Msg),      io:format("~s\n", [Msg]), lager:alert(Msg)).
-define(POPCORN_EMERGENCY_MSG(Msg),  io:format("~s\n", [Msg]), lager:emergency(Msg)).

-define(POPCORN_DEBUG_MSG(Msg, Args),      io:format("~s\n", [lists:flatten(io_lib:format(Msg, Args))]), lager:debug(Msg, Args)).
-define(POPCORN_INFO_MSG(Msg, Args),       io:format("~s\n", [lists:flatten(io_lib:format(Msg, Args))]), lager:info(Msg, Args)).
-define(POPCORN_NOTICE_MSG(Msg, Args),     io:format("~s\n", [lists:flatten(io_lib:format(Msg, Args))]), lager:notice(Msg, Args)).
-define(POPCORN_WARN_MSG(Msg, Args),       io:format("~s\n", [lists:flatten(io_lib:format(Msg, Args))]), lager:warning(Msg, Args)).
-define(POPCORN_ERROR_MSG(Msg, Args),      io:format("~s\n", [lists:flatten(io_lib:format(Msg, Args))]), lager:error(Msg, Args)).
-define(POPCORN_CRITICAL_MSG(Msg, Args),   io:format("~s\n", [lists:flatten(io_lib:format(Msg, Args))]), lager:critical(Msg, Args)).
-define(POPCORN_ALERT_MSG(Msg, Args),      io:format("~s\n", [lists:flatten(io_lib:format(Msg, Args))]), lager:alert(Msg, Args)).
-define(POPCORN_EMERGENCY_MSG(Msg, Args),  io:format("~s\n", [lists:flatten(io_lib:format(Msg, Args))]), lager:emergency(Msg, Args)).

-record(log_stream,  {stream_id       :: string(),
                      stream_pid      :: pid(),
                      client_pid      :: pid(),
                      applied_filters :: list(),
                      paused          :: boolean()}).

-record(popcorn_node, {node_name :: binary(),
                       role      :: binary(),
                       version   :: binary()}).

-record(log_message, {timestamp    :: number(),
                      severity     :: integer(),
                      message      :: binary(),
                      log_module   :: binary(),      %% underscore in the name is to prevent confusion with BIF and types
                      log_function :: binary(),
                      log_line     :: integer(),
                      log_pid      :: binary()}).

