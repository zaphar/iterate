-module(nitrogen_init).
-export ([init/0]).
	
%% Called during application startup.
%% Put other initialization code here.
init() ->
    application:start(mnesia)
    , application:start(nprocreg)
    , application:start(nitrogen_mochiweb)
.
