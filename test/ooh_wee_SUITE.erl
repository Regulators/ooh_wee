-module(ooh_wee_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(ezk),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].

all() ->
    [parallel_lock_grab,
     parallel_locks_grab,
     parallel_locks_grab_already_locked].

parallel_lock_grab(_Config) ->
    Self = self(),
    random:seed(now()),
    Path = "/ooh_wee_test" ++ integer_to_list(random:uniform(100000)),
    AttemptLockGrab =
        fun() ->
                Self ! (catch ooh_wee:lock(Path))
        end,
    L = length([spawn(AttemptLockGrab) || _ <- lists:seq(1, 10)]),
    Results = receive_n(L),
    {Locked, AlreadyLocked} =
        lists:partition(fun({ok, _}) ->
                                true;
                           ({error, already_locked}) ->
                                false
                        end,
                        Results),
    1 = length(Locked),
    9 = length(AlreadyLocked).

parallel_locks_grab(_Config) ->
    random:seed(now()),
    Paths = ["/ooh_wee_test" ++ integer_to_list(random:uniform(100000)) ||
                _ <- lists:seq(1, 10)],
    {ok, _Pid} = ooh_wee:mlock(Paths).

parallel_locks_grab_already_locked(_Config) ->
    random:seed(now()),
    [First| _] = Paths = ["/ooh_wee_test" ++ integer_to_list(random:uniform(100000)) ||
                _ <- lists:seq(1, 10)],
    {ok, _} = ooh_wee:lock(First),
    {error, {mlock_failed, First}} = ooh_wee:mlock(Paths).

receive_n(Count) ->
    receive_n(Count, []).

receive_n(0, L) ->
    L;
receive_n(Count, L) ->
    receive
        V ->
            receive_n(Count-1, [V|L])
    end.
