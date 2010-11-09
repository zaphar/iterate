-module(iterate_log).
-behaviour(gen_event).
-export([start/0, start_link/0, init/1, handle_event/2,
    handle_call/2, handle_info/2, terminate/2,
    code_change/3]).
-export([log_info/1, log_debug/1, log_warn/1, log_fatal/1, log/1]).
-export([get_log_location/0]).

start() ->
    io:format("Starting iterate_log server~n", [])
    , Return = gen_event:start_link({local, iterate_logger})
    , io:format("Started iterate_log server: [~p]~n", [Return])
    , gen_event:add_handler(iterate_logger, ?MODULE, [])
    , Return
.

start_link() ->
    start()
.

init(_Args) ->
    {ok, []}
.

handle_event({info, Msg}, State) ->
    log_it("INFO", Msg)
    , {ok, State}; 
handle_event({debug, Msg}, State) ->
    log_it("DEBUG", Msg)
    , {ok, State};
handle_event({warning, Msg}, State) ->
    log_it("WARNING", Msg)
    , {ok, State};
handle_event({fatal, Msg}, State) ->
    log_it("FATAL", Msg)
    , {ok, State} 
.

handle_call(view, State) ->
    {ok, {state, State}, State};
handle_call(_Call, State) ->
    {ok, State}
.

handle_info(_Info, State) ->
    {ok, State}
.

terminate(Args, State) ->
    try throw(a)
    catch throw:a ->
	io:format("STACKTRACE: ~p~n", [erlang:get_stacktrace()])
    end,
    io:format("Terminating iterate_logger: ~p ~n", [Args]),
    {ok, State}
.

code_change(_Args1, _Args2, State) ->
    {ok, State}
.

% TODO(jwall): these should all reuse the log call below
log_info(Msg) -> gen_event:notify(iterate_logger, {info, Msg}).
log_debug(Msg) -> gen_event:notify(iterate_logger, {debug, Msg}).
log_warn(Msg) -> gen_event:notify(iterate_logger, {warning, Msg}).
log_fatal(Msg) -> gen_event:notify(iterate_logger, {fatal, Msg}).

log({Type, Msg}) -> gen_event:notify(iterate_logger, {Type, Msg});
log(Msg) -> log({info, Msg}).

%% TODO(jwall): store filehandle in state?
log_it(Type, Msg) ->
    {ok, File} = get_log_location()
    , Epoch = date_util:now_to_milliseconds()
    , io:format(File, "~s: [~p] ~p~n", [Type, Epoch, Msg])
    , file:close(File)
.

get_log_location() ->
    case application:get_env(iterate, log_file) of
        undefined -> {ok, standard_io};
        {ok, Value} -> file:open(Value, [append])
    end
.

