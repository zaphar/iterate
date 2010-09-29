-module (index).
-include_lib ("nitrogen/include/wf.inc").
-include("records.hrl").
-include("events.hrl").
-compile(export_all).

main() ->
	%TODO(jwall): force identity
    case iterate_wf:working_as() of
        undefined ->
            #template { file="./site/templates/login.html"};
        _ ->
            #template { file="./site/templates/login.html"}
            %#template { file="./site/templates/template.html"}
    end
.

title() ->
	<<"Iterate!">>
.

display_title() ->
	<<"Iterate<i>!</i>">>
.

% TODO(jwall): should be abstracted to it's own module
login() ->
    #panel{ id=login, body=login_contents() }
.

login_contents() ->
    %comet2(start),
    User = case iterate_wf:working_as() of
        undefined ->
            <<"Please enter a name for yourself">>;
        Name ->
            Name
    end
    , [<<"Identity: ">>
       , #inplace_textbox{ text=User, tag=?IDENTIFY }]
.

inplace_textbox_event(?IDENTIFY, Value) ->
   login_update(Value) 
.

login_update(Value) ->
    iterate_log:log_info(wf:f("now working as: ~p~n", [Value]))
    %% TODO(jwall): users should get added to a db when they
    %% haven't been encountered before
    , wf:user(Value)
    , wf:redirect("/")
.

% comet needs a complete redesign
comet(start) ->
    wf:comet(fun() ->
               io:format("Comet handler function was called~n")
               , web_index:index_comet()
               , io:format("Comet handler function finished~n")
              end)
    , ""
.

comet2(start) ->
    wf:comet2(fun(_) ->
               io:format("Comet handler function was called~n")
               , web_index:index_comet2()
               , io:format("Comet handler function finished~n")
              end)
    , ""
.

index_comet2() ->
    undefined
    %io:format("running a comet update~n", [])
    %, timer:sleep(1000)
    %%, io:format("Finished sleeping~n", [])
    %, element_story_panel:update_story_list()
    %%, io:format("Updated story list~n", [])
    %, element_backlog_panel:update_backlog_panel()
    %%, io:format("Updated backlog list~n", [])
    %% flush because we are looping
    %%, io:format("Flushing comet update~n", [])
.

index_comet() ->
    undefined
    %index_comet2()
    %, wf:comet_flush()
    %%, io:format("Flushed comet update~n", [])
    %, index_comet()
.

event({click, {iteration, Name}}) ->
    element_story_panel:event(?SHOW_STORIES(iteration, Name))
.

