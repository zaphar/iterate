
-record(backlogs, {backlog_name, desc}).
-record(stories, {story_name, desc, sp, backlog=undefined, 
                  meta=[{ord, 0}
                        , {percent_complete, 0}
                       ]
                 }
).

