
-record(backlogs, {backlog_name, desc}).
-record(stories, {story_name, desc, sp, backlog="Default", 
                  meta=[{ord, 0}
                        , {percent_complete, 0}
                       ]
                 }
).

