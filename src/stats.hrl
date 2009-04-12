
-define(CREATE_STAT(Name), {creating, Name}).
-define(CHANGE_STAT(For, Type, Value), {change, For, {Type, Value}}).
-define(COMPLETE_STAT(Item, At), {complete, Item, {loc, At}}).
-define(CLOSE_STAT(Type, Item), {close, {Type, Item}}).
-define(MOVE_STAT(Item, To, From),
    {move, {Item, To, From}}).
-define(UPDATE_DESC_STAT(Name), {update_desc, Name}).
-define(DELETE_STAT(Name), {deleting, Name}).
