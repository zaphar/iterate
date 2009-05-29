-module(web_iterations).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("elements.hrl").

main() ->
    #template{ file="./wwwroot/iterations.html" }
.

title() -> "Iterate!".

display_title() -> "Iterate<i>!</i>".

%% a list of closed iterations
closed() -> "List".

%% a list of in_progress iterations
in_progress() -> "List".

comet(start) ->
    wf:comet(fun() -> comet() end)
    , ""
.

comet() ->
    timer:sleep(10*60*1000)
    %% flush because we are looping
    , wf:comet_flush()
    , comet()
.

