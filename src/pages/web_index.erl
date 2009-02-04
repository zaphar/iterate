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

%% TODO(jwall): change to an element module
backlog_panel() ->
    #rounded_panel{id=sidebar,
                   body=[
                        #label{text="iterate backlogs."},
                        #panel{id=iterate_backlogs,
                               body=[backlog_list()]
                        }
                   ]
    }. 

%% TODO(jwall): change to an element module
story_panel() ->
    #rounded_panel{ id=workspace, 
                    body=[story_list()]
    }.

%% TODO(jwall): change to an element module
backlog_list() ->
    #list{ id=backlog_list, numbered=ol, body=[
                backlog("Default")
                , backlog("Mine")
            ]
    }.

backlog(Name) when is_list(Name) ->
    %% TODO(jwall): change to using temp id's
    #panel{ id=Name
        , body=[ 
            #listitem{
                    %% TODO(jwall): change to an element module
                    body=[Name, " " 
                        , #link{text="Edit ", 
                                postback={show, {backlog, Name}}
                        }
                    ]
                    , actions=#event{type=click,
                                   postback={show, {stories, Name}}
                    } 
                    
            }
            , #panel{id=Name ++ "_target"}
        ]
    }.

%% TODO(jwall): change to an element module
story_list() ->
    #list{ id=story_list, body=[
        "click a backlog to see stories" 
    ]}.

%% TODO(jwall): change to an element module
story(Name) ->
    #listitem{ id=Name,
        body=[Name]
    }.

%% TODO(jwall): move events into a different module perhaps element modules?

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
