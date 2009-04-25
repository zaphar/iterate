-record(backlogs, {backlog_name, desc}).
-record(stories, {story_name, story_title, desc="", sp=3, backlog=undefined, 
                  meta=[{ord, 0}
                        , {percent_complete, 0}
                       ]
                 }
).
-record(tasks, {id, task_name, desc, story_name, complete=false}).
-record(time_log, {story, t_series=[]}).
-record(tags, {id, value, type, for}).
-record(iterations, {iteration_name, desc,
                     meta=[{started, false}
                     ]}
).
%% TODO(jwall): are epochs sufficiently unique?
-record(stats, {ts, for, user=undefined, entry}).

%% CRUD macros
-define(Q_ALL, {qry, all}).
-define(Q_STORY(Name), {qry, {story, Name}}).
-define(Q_UPDATE_STORY(Story), {update, Story}).
-define(Q_BACKLOG_STORY(Name), {qry, {backlog, Name}}).
-define(Q_ITERATION_STORY(Name), {qry, {iteration, Name}}).
-define(Q_BACKLOG(Name), {qry, Name}).
-define(Q_ITERATION(Name), {qry, Name}).
-define(Q_TAGS(Type, For), {qry, Type, For}).
-define(Q_MATCH_STAT(MatchSpec), {match, MatchSpec}).
-define(Q_FILTER_STATS(F), {filter, F}).
-define(NEWTAG(Type, For, Value), {new, {Type, For, Value}}).
-define(NEWITER(Name, Desc), {new, {iteration, Name, Desc}}).
-define(DELITER(Name), {delete, {iteration, Name}}).
-define(UPDATEITER(Iter), {update, {iteration, Iter}}).
-define(STOREITER(Iter), {store, {iteration, Iter}}).
-define(Q_STORY_TIME(Story), {qry, Story}).
-define(UPDATETIME(Story, Amount), {update, {log_time, {Story, Amount}}}).
-define(Q_AMT(Name), {calc_amt, Name}).
-define(Q_SEARCH_BACKLOG(Type, Term), {search, {Type, Term}}).
-define(Q_STORY_TASKS(For), {qry, {tasks, For}}).
-define(Q_TASK(Id), {qry, {task, Id}}).
-define(C_NEW_TASK(For, Name), {new, {task, For, Name}}).
-define(U_TASK(T), {update, {task, T}}).
-define(D_TASK(Id), {delete, {task, Id}}).

%% access macros
-define(BNAME(B), B#backlogs.backlog_name).
-define(BDESC(B), B#backlogs.desc).
-define(TVALUE(T), T#tags.value).

%% tagging macros
-define(TAG(Type,For,Value), #tags{id={Type, For, Value}, type=Type, for=For, value=Value}).
-define(STAG(S,V), #tags{id={story, S, V}, type=story, for=S, value=V}).
-define(BTAG(B,V), #tags{id={backlog, B, V}, type=backlog, for=B, value=V}).
