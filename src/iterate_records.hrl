
-record(backlogs, {backlog_name, desc}).
-record(stories, {story_name, desc, sp, backlog=undefined, 
                  meta=[{ord, 0}
                        , {percent_complete, 0}
                       ]
                 }
).
-record(time_log, {story, t_series=[]}).
-record(tags, {type, for, value}).

%% CRUD macros
-define(Q_STORY(Name), {qry, {story, Name}}).
-define(Q_BACKLOG(Name), {qry, Name}).
-define(Q_TAGS(Type, For), {qry, Type, For}).
-define(NEWTAG(Type, For, Value), {new, {Type, For, Value}}).

%% access macros
-define(BNAME(B), B#backlogs.backlog_name).

%% tagging macros
-define(TAG(Type,For,Value), #tags{type=Type, for=For, value=Value}).
-define(STAG(S,V), #tags{type=story, for=S, value=V}).
-define(BTAG(B,V), #tags{type=backlog, for=B, value=V}).

