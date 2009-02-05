-module (web_index).
-include_lib ("nitrogen/include/wf.inc").
-include("../elements.hrl").
-compile(export_all).

%%TODO(jwall): move the following into the main index page
main() ->
	#template { file="./wwwroot/template.html"}.

title() ->
	"Iterate<i>!</i>".

body() ->
    #singlerow{ id=main, cells=[
	    #tablecell{ body=[backlog_panel()] },
        #tablecell{ body=[story_panel()] }
    ]}.

backlog_panel() ->
    #rounded_panel{id=sidebar,
                   body=[
                        #label{text="backlogs."},
                        #panel{id=iterate_backlogs,
                               body=[backlog_list()]
                        }
                   ]
    }.

story_panel() ->
    #rounded_panel{ id=workspace,
                    body=[story_list()]
    }.

%% TODO(jwall): change to an element module
backlog_list() ->
    #list{ id=backlog_list, body=[
                backlog("Default")
                , backlog("Mine")
            ]
    }.

backlog(Name) when is_list(Name) ->
    %% TODO(jwall): change to using temp id's
    #panel{ id=Name
        , body=#listitem{ body=#backlog{backlog_name=Name} }
    }.

%% TODO(jwall): change to an element module
story_list() ->
    #list{ id=story_list, body=[
        "click a backlog to see stories"
    ]}.

story(Name) ->
    #listitem{ id=Name,
        body=#story{story_name=Name}
    }.

%% TODO(jwall): move events into a different module perhaps element modules?

%% showing stories
event({show, {stories, "Default"}}) ->
    wf:update(story_list, story("Story One"));
event({show, {stories, "Mine"}}) ->
    wf:update(story_list, [story("storage layer for backlogs and data")
                           , story("drag drop stories to backlogs")
                           , story("backlog filter and search")
                          ]);
event(_) -> ok.
