-module(iterate_sup).
-behaviour(supervisor).

-export([start/0, stop/0, init/1]).

start() ->
    supervisor:start_link(?MODULE, [])
.

stop() ->
    nitrogen:stop()
.

init(_) ->
    SupervisorArgs = {one_for_one, 1, 60}
    , NitrogenServer = {nitrogen, {nitrogen, start, []}
        , permanent, 1000, supervisor, [nitrogen]}
    , StatsServer = {iterate_stats, {iterate_stats, start, []}
        , permanent, brutal_kill, worker, dynamic}
    , {ok, {SupervisorArgs, [NitrogenServer, StatsServer]}} 
.


