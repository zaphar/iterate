% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module(element_my_inplace_textbox).
-include_lib("nitrogen/include/wf.inc").
-include("elements.hrl").

-compile(export_all).

reflect() -> record_info(fields, my_inplace_textbox).

render(ControlID, Record) -> 
	% Get vars...
	OKButtonID = wf:temp_id(),
	CancelButtonID = wf:temp_id(),
	ViewPanelID = wf:temp_id(),
	EditPanelID = wf:temp_id(),
	LabelID = wf:temp_id(),
	MouseOverID = wf:temp_id(),
	TextBoxID = wf:temp_id(),
	Tag = Record#my_inplace_textbox.tag,
	OriginalText = Record#my_inplace_textbox.text,
    Delegate = Record#my_inplace_textbox.delegate,

	% Set up the events...
	Controls = {ViewPanelID, LabelID, EditPanelID, TextBoxID},
	OKEvent = #event { delegate=?MODULE, postback={ok, Controls, Tag, Delegate} },
	CancelEvent = #event { delegate=?MODULE, postback={cancel, Controls, Tag, OriginalText} },
	
	% Create the view...
	Text = Record#my_inplace_textbox.text,
	Terms = #panel { 
		class="inplace_textbox " ++ wf:to_list(Record#my_inplace_textbox.class),
		style=Record#my_inplace_textbox.style,
		body = [
			#panel { id=ViewPanelID, style="display:inline;", class="view ", body=[
				#span { id=LabelID, class="label", text=Text, html_encode=Record#my_inplace_textbox.html_encode, actions=[
					#buttonize { target=ViewPanelID }
				]},
				#span { id=MouseOverID, class="instructions", text="Click to edit", actions=#hide{} }
			], actions = [
				#event { type=click, actions=[
					#hide { target=ViewPanelID },
					#show { target=EditPanelID },
					#script { script = wf:f("obj('~s').focus(); obj('~s').select();", [TextBoxID, TextBoxID]) }
				]},
				#event { type=mouseover, target=MouseOverID, actions=#show{} },
				#event { type=mouseout, target=MouseOverID, actions=#hide{} }
			]},
			#panel { id=EditPanelID, style="display:inline;", class="edit", body=[
				#textbox { id=TextBoxID, text=Text, next=OKButtonID },
				#button { id=OKButtonID, text="OK", actions=OKEvent#event { type=click } },
				#button { id=CancelButtonID, text="Cancel", actions=CancelEvent#event { type=click } }
			]}
		]
	},
	
	case Record#my_inplace_textbox.start_mode of
		view -> wf:wire(EditPanelID, #hide{});
		edit -> 
			wf:wire(ViewPanelID, #hide{}),
			wf:wire(TextBoxID, "obj('me').focus(); obj('me').select();")
	end,
	
	wf:wire(OKButtonID, TextBoxID, #validate { attach_to=CancelButtonID, validators=Record#my_inplace_textbox.validators }),
	
	element_panel:render(ControlID, Terms).

event({ok, {ViewPanelID, LabelID, EditPanelID, TextBoxID}, Tag, Delegate}) -> 
	[Value] = wf:q(TextBoxID),
    Value1 = Delegate:inplace_textbox_event(Tag, Value),
	wf:update(LabelID, Value1),
	wf:set(TextBoxID, Value1),
	wf:wire(EditPanelID, #hide {}),
	wf:wire(ViewPanelID, #show {}),
	ok;

event({cancel, {ViewPanelID, _LabelID, EditPanelID, TextBoxID}, _Tag, OriginalText}) ->
	wf:set(TextBoxID, OriginalText),
	wf:wire(EditPanelID, #hide {}),
	wf:wire(ViewPanelID, #show {}),
	ok;	

event(_Tag) -> ok.
