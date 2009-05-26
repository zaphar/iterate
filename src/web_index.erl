-module (web_index).
-include_lib ("nitrogen/include/wf.inc").
-include("elements.hrl").
-include("events.hrl").
-compile(export_all).

main() ->
	%TODO(jwall): force identity
    case iterate_wf:working_as() of
        undefined ->
            #template { file="./wwwroot/login.html"};
        _ ->
            #template { file="./wwwroot/template.html"}
    end
.

title() ->
	<<"Iterate!">>
.

display_title() ->
	<<"Iterate<i>!</i>">>
.

login() ->
    #panel{ id=login, body=login_contents() }
.

login_contents() ->
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

comet(start) ->
    wf:comet(fun() -> comet() end)
    , ""
.

comet() ->
    timer:sleep(10*60*1000)
    %% flush because we are looping
    , wf:comet_flush()
    , element_story_panel:update_story_list()
    , element_iteration_panel:update_iteration_panel()
    , comet()
.

