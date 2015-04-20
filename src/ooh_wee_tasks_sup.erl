-module(ooh_wee_tasks_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([start_child/2]).
-export([init/1]).

-define(SERVER, ?MODULE).


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(TaskGroup, Mod) ->
    Name = "/ooh_wee/tasks/" ++ TaskGroup,
    supervisor:start_child(?SERVER, [Name, Mod]).

init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    AChild = {ooh_wee_task_launcher, {ooh_wee_task_launcher, start_link, []},
              Restart, Shutdown, Type, [ooh_wee_task_launcher]},

    {ok, {SupFlags, [AChild]}}.
