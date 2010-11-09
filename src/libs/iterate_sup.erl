-module(iterate_sup).
-behaviour(supervisor).

-export([start/0, init/1, terminate/2, which_children/0]).

start() ->
    io:format("starting iterate_sup~n", [])
    , Return = supervisor:start_link({local, iterate_sup}, ?MODULE, [])
    , io:format("started iterate_sup: [~p]~n", [Return])
    , Return
.

init(_) ->
    io:format("in iterate_sup init~n", [])
    , SupervisorArgs = {one_for_one, 1, 60}
    , StatsServer = {iterate_stats, {iterate_stats, start, []}
        , permanent, 1000, worker, dynamic}
    , LogServer = {iterate_logger, {iterate_log, start, []}
        , permanent, 1000, worker, dynamic}
    , {ok, {SupervisorArgs, [StatsServer, LogServer]}} 
.

which_children() ->
    supervisor:which_children({iterate_sup, node()}).

terminate(_Args, _State) ->
    try throw(a)
    catch throw:a ->
	io:format("STACKTRACE: ~p~n", [erlang:get_stacktrace()])
    end,
    io:format("Terminating iterate_supervisor~n").
