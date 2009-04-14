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
            }
    end
    %, get_transformed(stories, F)
    , mnesia:transform_table(stories, F, record_info(fields, stories))
.

