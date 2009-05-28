-module (iterate_app).
-export ([start/2, stop/1, route/1, request/1]).
-behavior(application).

%%start(_, _) -> nitrogen:start().
start(_, _) -> iterate_sup:start().
%%stop(_) -> nitrogen:stop().
stop(_) -> iterate_sup:stop().

%% route/1 lets you define new URL routes to your web pages, 
%% or completely create a new routing scheme.
%% The 'Path' argument specifies the request path. Your
%% function should return either an atom which is the page module
%% to run, or a tuple containing {Module, PathInfo}. PathInfo
%% can be accessed using wf:get_path_info(). 
%%
%% Uncomment the line below to direct requests 
%% from "/web/newroute" to the web_index module:
%%
%% route("/web/newroute") -> web_index;
%%
%% Uncomment the line below to direct requests 
%% from "/web/newroute" to the web_index module, 
%% with trailing PathInfo included:
%%
%% route("/web/newroute/" ++ PathInfo) -> {web_index, PathInfo};

route(Path) -> 
    iterate_log:log_debug(wf:f("getting route for: ~p", [Path]))
    , nitrogen:route(Path)
.


%% request/1 is executed before every Nitrogen page, and lets
%% you add authentication and authorization. The 'Module' argument
%% is the name of the page module.
%% This function should return either 'ok' if processing can proceed,
%% or it can return a full-fledged page by treating it just like the main function
%% of a page. Alternatively, you can use the wf:redirect* functions to 
%% issue a client-side redirect to a new page.

request(web_login) ->
    nitrogen:request(web_login);
request(Module) -> 
    iterate_log:log_debug(wf:f("handling request for: ~p", [Module]))
    , case iterate_wf:working_as() of
        undefined ->
            wf:redirect("/web/login");
        _ ->
            nitrogen:request(Module)
    end
.
