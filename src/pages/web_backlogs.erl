-module (web_backlogs).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() -> 
	#template { file="./wwwroot/template.html"}.

title() ->
	"web_backlogs".

body() ->
    #panel{ id=main, body=[
	    #rounded_panel{ id=sidebar, body=[
                      #label{text="iterate backlogs."},
                      #sortblock{id=iterate_backlogs,
                                 items=[backlog_list()]
                                }
                   ]
        },
        story_list()
    ]}.

backlog_list() ->
    #panel{ id=backlog_list, body=[
                backlog("default"),
                backlog("Mine")
            ]
    }.

backlog(Name) when is_list(Name) ->
    #sortitem{ id=Name,
        body=[Name],
        actions=#event{type=click, postback={show, {stories, Name}}}
    }.

story_list() ->
    #sortblock{ id=story_list, items=[
               "click a backlog to see stories" 
            ]
          }.

story(Name) ->
    #sortitem{ tag=Name,
        body=[Name]
    }.
    
drop_event(_, _) ->
    ok.

event({show, {stories, "default"}}) ->
    io:format("handling event ~n", []),
    wf:update(story_list, story("Story One"));
event({show, {stories, "Mine"}}) ->
    io:format("handling event ~n", []),
    wf:update(story_list, story("Story Two"));
event(_) -> ok.
