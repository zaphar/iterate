-module (web_index).
-include_lib ("nitrogen/include/wf.inc").
-include("elements.hrl").
-compile(export_all).

%%TODO(jwall): move the following into the main index page
main() ->
	#template { file="./wwwroot/template.html"}
.

title() ->
	"Iterate!"
.

display_title() ->
	"Iterate<i>!</i>"
.

body() ->
    #panel{ id=main, body=[
        #hr{}
        , body(contents)
    ]}
.

body(contents) ->
    #singlerow{ id=main, cells=[
	    #tablecell{ body=[iteration_panel()
            , backlog_panel()] }
        , #tablecell{ body=[story_panel()] }
    ]}
.

backlog_panel() ->
    #backlog_panel{data=iterate_db:backlogs()}
.

iteration_panel() ->
    #iteration_panel{data=iterate_db:iterations()}
.

story_panel() ->
    #story_panel{ data=[] }
.

event(Event) -> 
    io:format("~p recieved event: ~p~n", [?MODULE, Event]),
    ok
.

