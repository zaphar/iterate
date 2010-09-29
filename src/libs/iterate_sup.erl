-module(iterate_sup).
-behaviour(supervisor).

-export([start/0, init/1]).

start() ->
    supervisor:start_link(?MODULE, [])
.

init(_) ->
    SupervisorArgs = {one_for_one, 1, 60}
    , StatsServer = {iterate_stats, {iterate_stats, start, []}
        , permanent, brutal_kill, worker, dynamic}
    , LogServer = {iterate_logger, {iterate_log, start_link, []}
        , permanent, brutal_kill, worker, dynamic}
    , {ok, {SupervisorArgs, [StatsServer, LogServer]}} 
.
