-module(element_sparkline).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("elements.hrl").

reflect() ->
    record_info(fields, my_sparkline)
.

rec() ->
    #my_sparkline{}
.

mk_series(Data) ->
    string:join(lists:map(fun(I) -> io_lib:format("~B", [I]) end, Data), ", ")    
.

mk_opts(Opts) ->
    "{" ++ string:join(lists:map(fun({Opt, Val}) -> io_lib:format("~s : ~s", [Opt, Val]) end, Opts), ", ")
    ++ "}"
.

mk_script(Id, [], Opts) ->
    wf:f("$('#~s').sparkline(~s);", [Id, mk_opts(Opts)]);
mk_script(Id, Data, Opts) ->
    wf:f("$('#~s').sparkline([~s], ~s);", [Id, mk_series(Data), mk_opts(Opts)])
.

render(Target, R) ->
    Script = mk_script(Target, R#my_sparkline.series
        , [{composite, R#my_sparkline.composite}])
    , Span = #span{id=Target}
    , wf:wire(Target, Script)
    , wf:render(Span)
.
