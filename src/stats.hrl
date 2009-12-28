
-define(CREATE_STAT(Name), {creating, Name}).
-define(CHANGE_STAT(For, Type, Value), {change, For, {Type, Value}}).
-define(COMPLETE_STAT(Item, At), {complete, Item, {loc, At}}).
-define(CLOSE_STAT(Type, Item), {close, Item, Type}).
-define(MOVE_STAT(Item, To, From),
    {move, Item, {To, From}}).
-define(MOVE_STAT_OLD(Item, To, From),
    {move, {Item, To, From}}).
-define(UPDATE_DESC_STAT(Name), {update_desc, Name}).
-define(DELETE_STAT(Name), {deleting, Name}).

-record(tsentry, {epoch, date, time, type, value=0}).
-define(TIMESERIES(Epoch, Date, Time, Value, Type), #tsentry{epoch=Epoch
    , date=Date, time=Time, type=Type, value=Value}).

