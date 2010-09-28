-define(SPANELID, "story_list").

%% backlog element event tags
-define(UPDATE_B_EL(Name, ElId), {update, {backlog, Name, ElId}}).
-define(SHOW_B_EL(Name, ElId), {show, {backlog, Name, ElId}}).
-define(REMOVE_B_EL(Name, ElId), {remove, {backlog, Name, ElId}}).
-define(DELETE_B_EL(Name, ElId), {delete, {backlog, Name, ElId}}).

%% backlog edit element event tags
-define(UPDATENAME(Name, Id), {backlog, {update, {backlog_name, Name}, {id, Id}}}).
-define(UPDATEDESC(Name), {backlog, {update, {backlog_desc, Name}}}).

%% backlog panel event tags
-define(CREATE_B(Id, PanelId), {create, {new, backlog}, Id, PanelId}).
-define(B_PANEL_CREATE(Id), {create, backlog, Id}).
-define(B_PANEL_SEARCH(Id, PanelId), {search, Id, PanelId}).
-define(B_EDIT_SHOW(Name), {show, {backlog, Name}}).
-define(B_EDIT_REMOVE(Name), {remove, {backlog, Name}}).

%% story_panel event tags
-define(CREATE_S(Id, PanelId, Backlog), {create, {new, story},
    {backlog, Backlog}, Id, PanelId}).
-define(S_PANEL_CREATE(Type, For), {create, story, {Type, For}}).
-define(SHOW_STORIES(Type, Name), {show, {stories, {Type, Name}}}).

%% story element event tags
-define(SHOW_S_EL(Name), {show, {story, Name}}).
-define(REMOVE_S_EL(Name), {remove, {story, Name}}).
-define(DELETE_S_EL(Name, ElId), {delete, {story, Name, ElId}}).
-define(COMPLETE_S(Name), {complete, {story, Name}}).
-define(ORDEDIT(Name, Id), {update, order, {story, Name, Id}}).

%% story editing event tags
-define(UPDATESP(Id), {update, sp, Id}).
-define(UPDATE_S_DESC(Id), {update, desc, Id}).
-define(UPDATE_T_LOG(Name), {update, {story, Name, time_log}}).
-define(NEWTIME(Name, Id, PanelId), {newtime, Name, Id, PanelId}).
-define(TAGCHANGE(For), {update, {tag, For}}).

%% iteration event tags
-define(CLOSE_I_EL(Name, PanelId), 
    {close, {iteration, Name}
        , {panel, PanelId}}).

%% task event tags
-define(COMPLETE_TASK(CBId, Id), {complete
    , {task, Id}
    , {checkbox, CBId}}).
-define(DELETE_TASK(Id, PanelId, Story), {delete
    , {task, Id}
    , {panel, PanelId}
    , {story, Story}}).

%% misc shared event tags
-define(REFRESH(Id), {refresh, Id}).
-define(REFRESH_TYPE(Id, Type), {refresh, Id, Type}).
-define(STARTITER(PanelId), {start, iteration, {panel, PanelId}}).
-define(STARTITERTNAME(TBId, PanelId, IterPanelId),
    {start, iteration, TBId, PanelId, IterPanelId}).

-define(IDENTIFY, identify).

