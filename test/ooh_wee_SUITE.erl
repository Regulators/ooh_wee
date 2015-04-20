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
    [parallel_lock_grab].

parallel_lock_grab(_Config) ->
    Self = self(),
    random:seed(now()),
    Path = "/ooh_wee_test" ++ integer_to_list(random:uniform(100000)),
    AttemptLockGrab =
        fun() ->
                Self ! (catch ooh_wee_lock:lock(Path))
        end,
    L = length([spawn(AttemptLockGrab) || _ <- lists:seq(1, 10)]),
    Results = receive_n(L),
    L2 = L-1,
    {1, L2} = only_one({ok, locked}, {error, already_locked}, Results).

receive_n(Count) ->
    receive_n(Count, []).

receive_n(0, L) ->
    L;
receive_n(Count, L) ->
    receive
        V ->
            receive_n(Count-1, [V|L])
    end.

only_one(Value, Rest, L) ->
    only_one(Value, Rest, L, 0, 0).

only_one(_Value, _Rest, [], N, N2) ->
    {N, N2};
only_one(Value, Rest, [Value|T], N, N2) ->
    only_one(Value, Rest, T, N+1, N2);
only_one(Value, Rest, [Rest|T], N, N2) ->
    only_one(Value, Rest, T, N, N2+1).
