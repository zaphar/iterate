
-define(BPANELID, "backlog_panel").

%% backlog element event tags
-define(UPDATE_B_EL(Name, ElId), {update, {backlog, Name, ElId}}).
-define(SHOW_B_EL(Name, ElId), {show, {backlog, Name, ElId}}).
-define(REMOVE_B_EL(Name, ElId), {remove, {backlog, Name, ElId}}).

%% backlog edit element event tags
-define(UPDATENAME(Name, Id), {backlog, {update, {backlog_name, Name}, {id, Id}}}).
-define(UPDATEDESC(Name), {backlog, {update, {backlog_desc, Name}}}).

%% story editing event tags
-define(UPDATESP(Id), {update, sp, Id}).
-define(UPDATE_S_DESC(Id), {update, desc, Id}).

%% backlog_panel event tags
-define(CREATE_B(Id, PanelId), {create, {new, backlog}, Id, PanelId}).
-define(B_PANEL_CREATE, {create, backlog}).
