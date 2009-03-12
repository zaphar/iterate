
-record(backlogs, {backlog_name, desc}).
-record(stories, {story_name, desc, sp, backlog=undefined, 
                  meta=[{ord, 0}
                        , {percent_complete, 0}
                       ]
                 }
).
-record(time_log, {story, t_series=[]}).
-record(tags, {value, type, for}).
-record(iterations, {iteration_name, desc,
                     meta=[{started, false}
                     ]}
).

%% CRUD macros
-define(Q_ALL, {qry, all}).
-define(Q_STORY(Name), {qry, {story, Name}}).
-define(Q_BACKLOG_STORY(Name), {qry, {backlog, Name}}).
-define(Q_ITERATION_STORY(Name), {qry, {iteration, Name}}).
-define(Q_BACKLOG(Name), {qry, Name}).
-define(Q_ITERATION(Name), {qry, Name}).
-define(Q_TAGS(Type, For), {qry, Type, For}).
-define(NEWTAG(Type, For, Value), {new, {Type, For, Value}}).
-define(NEWITER(Name, Desc), {new, {iteration, Name, Desc}}).
-define(DELITER(Name), {delete, {iteration, Name}}).
-define(UPDATEITER(Iter), {update, {iteration, Iter}}).
-define(STOREITER(Iter), {store, {iteration, Iter}}).
-define(Q_STORY_TIME(Story), {qry, Story}).
-define(UPDATETIME(Story, Amount), {update, {log_time, {Story, Amount}}}).

%% access macros
-define(BNAME(B), B#backlogs.backlog_name).
-define(TVALUE(T), T#tags.value).

%% tagging macros
-define(TAG(Type,For,Value), #tags{type=Type, for=For, value=Value}).
-define(STAG(S,V), #tags{type=story, for=S, value=V}).
-define(BTAG(B,V), #tags{type=backlog, for=B, value=V}).

