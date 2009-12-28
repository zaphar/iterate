-module(iterate_stats).
-behaviour(gen_event).
-export([start/0, init/1, handle_event/2,
    handle_call/2, handle_info/2, terminate/2,
    code_change/3]).
-export([record/2]).

start() ->
    Return = gen_event:start_link({local, stats_logger})
    , gen_event:add_handler(stats_logger, ?MODULE, [])
    , Return
.

init(_Args) ->
    {ok, []}
.

%% handle the stats events
handle_event({Type, User, Entry}, State) ->
    Result = iterate_db:new_stat(Type, Entry, User)
    , iterate_log:log_info(wf:f("creating a new stat: ~p", [Result]))
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

user() -> iterate_wf:working_as().

%% TODO(jwall): standardize the entry format
record(Type, Entry) ->
    gen_event:notify(stats_logger, {Type, user(), Entry})
.

