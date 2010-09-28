-module(iterations).
-include_lib ("nitrogen/include/wf.inc").
-include("records.hrl").
-compile(export_all).

main() ->
    #template{ file="./site/templates/iterations.html" }
.

title() -> "Iterate!".

display_title() -> "Iterate<i>!</i>".

%% a list of closed iterations
all() ->
    comet(start),
    element_iteration_panel:render(all)
.

log_details() ->
    case iterate_wf:working_in() of
        {iteration, Name} ->
            #panel{id=iteration_log, body=log_details(Name)}; 
        _ ->
            #panel{id=iteration_log, body="Select an iteration to see it's history"} 
    end
.

log_details(For) ->
    wf:f("Log for: ~s<pre>~n~p</pre>"
        , [For, iterate_db:stat({for, For})])
    
.

comet(start) ->
    wf:comet(fun() -> comet() end)
    , ""
.

comet() ->
    timer:sleep(10*60*1000)
    , element_iteration_panel:refresh(closed)
    % flush because we are looping
    %, wf:comet_flush()
    , comet()
.

event({click, {iteration, Name}}) ->
    wf:update(iteration_log, log_details(Name))

.

