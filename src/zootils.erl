-module(zootils).

-export([lock/1]).
-export([unlock/2]).
-export([munlock/2]).
-export([mlock/1]).

mlock(Paths) ->
    Servers = application:get_env(zootils, zookeeper_servers, []),
    case ezk:start_connection(Servers) of
        {ok, Pid} ->
            mlock(Pid, Paths);
        {error, no_server_reached} ->
            {error, no_zk_connection}
    end.

mlock(Pid, []) ->
    {ok, Pid};
mlock(Pid, [Path|Rest]) ->
    case lock(Pid, Path) of
        {ok, Pid} ->
            mlock(Pid, Rest);
        {error, already_locked} ->
            ok = ezk:end_connection(Pid, mlock_failed),
            {error, {mlock_failed, Path}};
        {error, no_zk_connection} = Error ->
            Error
    end.

lock(Path) ->
    Servers = application:get_env(zootils, zookeeper_servers, []),
    case ezk:start_connection(Servers) of
        {ok, Pid} ->
            lock(Pid, Path);
        {error, no_server_reached} ->
            {error, no_zk_connection}
    end.

lock(Pid, Path) ->
    case grab_lock(Pid, Path) of
        {ok, locked} ->
            {ok, Pid};
        {error, already_locked} ->
            {error, already_locked};
        {error, {no_zk_connection = ZKError, _}} ->
            {error, ZKError}
    end.

grab_lock(Pid, Path) when is_pid(Pid) andalso is_list(Path) ->
    NodeBin = atom_to_binary(node(), latin1),
    case ezk:create(Pid, Path, NodeBin, e) of
        {ok, Path} ->
            {ok, locked};
        {error, dir_exists} ->
            {error, already_locked}
    end.

munlock(Pid, Paths) when is_pid(Pid) andalso is_list(Paths) ->
    [{ok, _} = unlock(Pid, Path) || Path <- Paths],
    {ok, Pid}.

unlock(Pid, Path) when is_pid(Pid) andalso is_list(Path) ->
    case ezk:delete(Pid, Path) of
        {ok, Path} ->
            {ok, unlocked};
        {error, no_dir} ->
            {ok, unlocked};
        {error, {no_zk_connection = ZKError, _}} ->
            {error, ZKError}
    end.
