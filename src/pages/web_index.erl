-module (web_index).
-include_lib ("nitrogen/include/wf.inc").
-include("../elements.hrl").
-compile(export_all).

%%TODO(jwall): move the following into the main index page
main() -> 
	#template { file="./wwwroot/template.html"}.

title() ->
	"Backlogs".

body() ->
    #singlerow{ id=main, cells=[
	    #tablecell{ body=[backlog_panel()] },
        #tablecell{ body=[story_panel()] }
    ]}.

backlog_panel() ->
    #rounded_panel{id=sidebar,
                   body=[
                        #label{text="iterate backlogs."},
                        #panel{id=iterate_backlogs,
                               body=[backlog_list()]
                        }
                   ]
    }. 

story_panel() ->
    #rounded_panel{ id=workspace, 
                    body=[story_list()]
    }.

backlog_list() ->
    #list{ id=backlog_list, numbered=ol, body=[
                backlog("Default")
                , backlog("Mine")
            ]
    }.

backlog(Name) when is_list(Name) ->
    #panel{ id=Name
        , body=[ 
            #listitem{
                    body=[Name, " " 
                        , #link{text="Edit ", postback={show, {backlog, Name}}}
                    ],
                    actions=#event{type=click,
                               postback={show, {stories, Name}}
                    } 
                    
            }
            , #panel{id=Name ++ "_target"}
        ]
    }.

story_list() ->
    #list{ id=story_list, body=[
        "click a backlog to see stories" 
    ]}.

story(Name) ->
    #listitem{ id=Name,
        body=[Name]
    }.

%% TODO(jwall): move events into a different module?

%% showing stories
event({show, {stories, "Default"}}) ->
    wf:update(story_list, story("Story One"));
event({show, {stories, "Mine"}}) ->
    wf:update(story_list, story("Story Two"));

%% showing backlog info
event({show, {backlog, Name}}) ->
    wf:update(Name ++ "_target", #backlog_el{backlog_id=Name, desc="A description"});
event({remove, {backlog, Name}}) ->
    wf:update(Name ++ "_target", "");
event(_) -> ok.
