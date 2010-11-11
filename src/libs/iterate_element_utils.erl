-module(iterate_element_utils).
-compile(export_all).
-include_lib("nitrogen/include/wf.inc").

autofocus_text_box(Id, Default) ->
    #textbox{ id=Id, text=Default, actions=#script{
        script=wf:f("var box = obj('~s');box.focus();box.select();"
            , [Id])}}
.

autofocus_text_box(Id, Default, Actions) when is_list(Actions) ->
    #textbox{ id=Id, text=Default, actions=[#script{
        script=wf:f("var box = obj('~s');box.focus();box.select();"
                , [Id])}| Actions]}
;
autofocus_text_box(Id, Default, Action) ->
    #textbox{ id=Id, text=Default, actions=[#script{
        script=wf:f("var box = obj('~s');box.focus();box.select();"
                , [Id])}, Action]}
.

is_whitespace(C) -> lists:any(fun (C2) -> C2 == C end, " \n\r\t").

%% because for reasons I have not yet determined 2.0 is sensitive to whitespace
normalize_id(L) -> lists:filter(fun (C) -> not is_whitespace(C) end, L).
