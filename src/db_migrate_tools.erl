-module(db_migrate_tools).
-compile(export_all).

-include("iterate_records.hrl").
-include_lib("stdlib/include/qlc.hrl").

get_transformed(T, F) ->
    Trans = fun() ->
        QH = qlc:q([F(R) || R <- mnesia:table(T)])
        , qlc:eval(QH)
    end
    , case mnesia:transaction(Trans) of
        {atomic, RecordList} ->
            RecordList;
        {abort, _} ->
            throw("failed to get transformed records")
    end
.

transform_stories() ->
    F = fun
        %% V1 records upgraded to current format
        ({stories, StoryName, Desc, Sp, Backlog, Meta}) ->
            #stories{story_name=StoryName
                , story_title=StoryName
                , desc=Desc
                , sp=Sp
                , backlog=Backlog
                , meta=Meta
            };
        %% if any titles or descriptions are in string format we
        %% want them to binary from now on
        ({stories, StoryName, StoryTitle, Desc, Sp, Backlog, Meta})
            when is_list(StoryTitle) orelse is_list(Desc) ->
                Story = case is_list(StoryTitle) of
                    true -> 
                        #stories{story_name=StoryName
                            , story_title=list_to_binary(StoryTitle)
                            , desc=Desc
                            , sp=Sp
                            , backlog=Backlog
                            , meta=Meta
                        };
                    false ->
                        #stories{story_name=StoryName
                            , story_title=StoryTitle
                            , desc=Desc
                            , sp=Sp
                            , backlog=Backlog
                            , meta=Meta
                        }
                end
                , case is_list(Story#stories.desc) of
                    true ->
                        #stories{story_name=StoryName
                            , story_title=StoryTitle
                            , desc=list_to_binary(Desc)
                            , sp=Sp
                            , backlog=Backlog
                            , meta=Meta
                        };
                    false ->
                        #stories{story_name=StoryName
                            , story_title=StoryTitle
                            , desc=Desc
                            , sp=Sp
                            , backlog=Backlog
                            , meta=Meta
                        }
                end
    end
    , mnesia:transform_table(stories, F, record_info(fields, stories))
.

