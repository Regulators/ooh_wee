-module(ooh_wee_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Servers = application:get_env(ooh_wee, zookeeper_servers, []),
    ok = case ezk:start_connection(Servers) of
             {ok, Pid} ->
                 ok = zk_utils:is_ok_create(ezk:create(Pid, "/ooh_wee", <<>>)),
                 ok = zk_utils:is_ok_create(ezk:create(Pid, "/ooh_wee/tasks", <<>>));
             {error, _} = E->
                 E
         end,
    ooh_wee_sup:start_link().

stop(_State) ->
    ok.
