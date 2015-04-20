-module(ooh_wee_tasks).


-export([submit_task/3]).
-export([handle_tasks/2]).


submit_task(Pid, TaskGroup, TaskData) when is_list(TaskGroup) ->
    TaskPath = "/ooh_wee/tasks/" ++ TaskGroup ++ "/task-",
    zk_utils:is_ok_create(ezk:create(Pid, "/ooh_wee/tasks/" ++ TaskGroup, <<>>)),
    ezk:create(Pid, TaskPath, TaskData, s).

handle_tasks(TaskGroup, Mod)
  when is_list(TaskGroup) andalso is_atom(Mod) ->
    {ok, _} = ooh_wee_tasks_sup:start_child(TaskGroup, Mod),
    ok.
