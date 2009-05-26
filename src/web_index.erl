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
            <<"enter an identity">>;
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
    io:format("now working as: ~p~n", [Value])
    , wf:user(Value)
    , wf:redirect("/")
.

