
-record(backlogs, {backlog_name, desc}).
-record(stories, {story_name, desc, sp, backlog=undefined, 
                  meta=[{ord, 0}
                        , {percent_complete, 0}
                       ]
                 }
).
-record(time_log, {story, t_series=[]}).
