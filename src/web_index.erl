-module (web_index).
-include_lib ("nitrogen/include/wf.inc").
-include("elements.hrl").
-include("events.hrl").
-compile(export_all).

main() ->
	#template { file="./wwwroot/template.html"}
.

title() ->
	"Iterate!"
.

display_title() ->
	"Iterate<i>!</i>"
.

login() ->
    #panel{ id=login, body=login_contents() }
.

login_contents() ->
    User = case wf:user() of
        undefined ->
            "enter an identity";
        Name ->
            Name
    end
    , ["Identity: "
       , #inplace_textbox{ text=User, tag=?IDENTIFY }]
.

% TODO(jwall): change this to be like the story panel
backlog_panel() ->
    #backlog_panel{data=iterate_db:backlogs()}
.

iteration_panel() ->
    element_iteration_panel:render()
.

story_panel() ->
    element_story_panel:render()
.

event(Event) -> 
    io:format("~p recieved event: ~p~n", [?MODULE, Event]),
    ok
.

inplace_textbox_event(?IDENTIFY, Value) ->
   login_update(Value) 
.

login_update(Value) ->
    io:format("now working as: ~p~n", [Value])
    , wf:user(Value)
.

