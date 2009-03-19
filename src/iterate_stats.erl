-module(iterate_stats).
-behaviour(gen_event).
-export([start/0, init/1, handle_event/2,
    handle_call/2, handle_info/2, terminate/2,
    code_change/3, user/0]).
-export([record/2]).

start() ->
    gen_event:start_link({local, stats_logger})
.

init(_Args) ->
    gen_event:add_handler(stats_logger, iterate_stats, [])
.

%% handle the stats events
handle_event({For, User, Entry}, State) ->
    Result = iterate_db:new_stat(For, Entry, User)
    , io:format("creating a new stat: ~p", [Result])
    , {ok, State} 
.

handle_call(_Call, State) ->
    {ok, State}
.

handle_info(_Info, State) ->
    {ok, State}
.

terminate(_Args, State) ->
    {ok, State}
.

code_change(_Args1, _Args2, State) ->
    {ok, State}
.

%% return current nitrogen user or undefined
user() ->
    case catch wf:user() of
        {'EXIT', _Err} ->
            undefined;
        User ->
            User
    end
.

record(For, Entry) ->
    gen_event:notify(stats_logger, {For, user(), Entry})
.

