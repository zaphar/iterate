-module (web_index).
-include_lib ("nitrogen/include/wf.inc").
-include("elements.hrl").
-include("events.hrl").
-compile(export_all).

main() ->
	%TODO(jwall): force identity
    #template { file="./wwwroot/login.html"}
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
