
-define(BPANELID, "backlog_panel").
-define(SPANELID, "story_list").

%% backlog element event tags
-define(UPDATE_B_EL(Name, ElId), {update, {backlog, Name, ElId}}).
-define(SHOW_B_EL(Name, ElId), {show, {backlog, Name, ElId}}).
-define(REMOVE_B_EL(Name, ElId), {remove, {backlog, Name, ElId}}).
-define(DELETE_B_EL(Name, ElId), {delete, {backlog, Name, ElId}}).

%% backlog edit element event tags
-define(UPDATENAME(Name, Id), {backlog, {update, {backlog_name, Name}, {id, Id}}}).
-define(UPDATEDESC(Name), {backlog, {update, {backlog_desc, Name}}}).

%% backlog_panel event tags
-define(CREATE_B(Id, PanelId), {create, {new, backlog}, Id, PanelId}).
-define(B_PANEL_CREATE, {create, backlog}).

%% story_panel event tags
-define(CREATE_S(Id, PanelId, Backlog), {create, {new, story},
    {backlog, Backlog}, Id, PanelId}).
-define(S_PANEL_CREATE(B), {create, story, B}).
-define(SHOW_STORIES(Name), {show, {stories, Name}}).

%% story element event tags
-define(SHOW_S_EL(Name), {show, {story, Name}}).
-define(REMOVE_S_EL(Name), {remove, {story, Name}}).
-define(DELETE_S_EL(Name, ElId), {delete, {story, Name, ElId}}).

%% story editing event tags
-define(UPDATESP(Id), {update, sp, Id}).
-define(UPDATE_S_DESC(Id), {update, desc, Id}).

%% query macros
-define(Q_STORY(Name), {qry, {story, Name}}).

