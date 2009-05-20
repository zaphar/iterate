% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_delegated_droppable).
-include_lib("nitrogen/include/wf.inc").
-include("elements.hrl").
-compile(export_all).

reflect() -> record_info(fields, delegated_droppable).

render(ControlID, Record) -> 
	% Get properties...
	PickledPostbackInfo = action_event:make_postback_info(Record#delegated_droppable.tag, sort, ControlID, ControlID, ?MODULE),
	ActiveClass = Record#delegated_droppable.active_class, 
	HoverClass = Record#delegated_droppable.hover_class,
	AcceptGroups = groups_to_accept(Record#delegated_droppable.accept_groups),

	% Write out the script to make this element droppable...
	Script = wf:f("Nitrogen.$droppable(obj('~s'), { activeClass: '~s', hoverClass: '~s', accept: '~s' }, '~s');", [
		ControlID, 
		ActiveClass, 
		HoverClass, 
		AcceptGroups, 
		PickledPostbackInfo
	]),
	wf:wire(Script),

	% Render as a panel.
	element_panel:render(ControlID, #panel {
		class="droppable " ++ wf:to_list(Record#delegated_droppable.class),
		style=Record#delegated_droppable.style,
		body=Record#delegated_droppable.body
	}).

event({DropTag, {delegate, Module}}) ->
	[DragItem] = wf:q(drag_item),
	DragTag = wf:depickle(DragItem),
	Module:drop_event(DragTag, DropTag);
event(DropTag) ->
	Module = wf_platform:get_page_module(),
	event({DropTag, {delegate, Module}}).

groups_to_accept(all) -> "*";
groups_to_accept(undefined) -> "*";
groups_to_accept(none) -> "";
groups_to_accept([]) -> "*";
groups_to_accept(Groups) ->
	Groups1 = lists:flatten([Groups]),
	Groups2 = [".drag_group_" ++ wf:to_list(X) || X <- Groups1],
	string:join(Groups2, ", ")
.

