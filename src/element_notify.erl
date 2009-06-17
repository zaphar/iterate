-module(element_notify).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("elements.hrl").

reflect() -> record_info(fields, notify).

render() ->
    wf:render(#panel{id=notification_area})
.

-define(HIDE(Type, Delay, Id), #event{type=Type, delay=Delay
    , actions=#hide{effect=blind, target=Id, speed=2000}}).

render(ControlId, R) when is_record(R, notify) ->
    Id = ControlId
    , case R#notify.expire of
        false ->
            undefined;
        N when is_integer(N) ->
            % we expire in this many seconds
            wf:wire(Id, ?HIDE('timer', N, Id));
        Err ->
            % log error and don't expire
            iterate_log:log_warning(wf:f("encountered unknown expire value: ~p"
                , [Err]))
            , undefined
    end
    , case R#notify.closebtn of
        undefined ->
            undefined;
        Btn ->
            % we expire in this many seconds
            wf:wire(Btn, ?HIDE('click', 0, Id))
    end
    , case R#notify.evt of
        undefined ->
            undefined;
        {Evt, Source} ->
            % we expire in this many seconds
            wf:wire(Source, ?HIDE(Evt, 0, Id))
    end
    , Link = #link{text="dismiss", actions=?HIDE(click, undefined, Id)}
    , InnerPanel = #panel{class="notify_inner", body=R#notify.msg}
    , Panel = #panel{id=Id
        , class=["notify ", R#notify.class]
        , body=#singlerow{ 
            cells=[#tablecell{align=left, body=InnerPanel}
                , #tablecell{align=right, body=Link}]}
    }
    , element_panel:render(Id, Panel)
.

msg(Content) ->
    wf:insert_bottom(notification_area, #notify{msg=Content})
.

msg(Content, {event, {Evt, Id}}) ->
    wf:insert_bottom(notification_area, #notify{msg=Content, evt={Evt, Id}});
msg(Content, {close, Close}) ->
    wf:insert_bottom(notification_area, #notify{msg=Content, closebtn=Close});
msg(Content, Expire) when is_integer(Expire) ->
    wf:insert_bottom(notification_area, #notify{msg=Content, expire=Expire})
.
