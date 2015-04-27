-module(zootils).

-export([lock/1]).
-export([lock/2]).
-export([unlock/2]).
-export([munlock/2]).
-export([mlock/1]).
-export([mlock/2]).


mlock(Paths) ->
    mlock(Paths, []).

mlock(Paths, Opts) ->
    Servers = application:get_env(zootils, zookeeper_servers, []),
    case ezk:start_connection(Servers) of
        {ok, Pid} ->
            mlock(Pid, Paths, Opts);
        {error, no_server_reached} ->
            {error, no_zk_connection}
    end.

mlock(Pid, [], _Opts) ->
    {ok, Pid};
mlock(Pid, [Path|Rest], Opts) ->
    case lock(Pid, Path, Opts) of
        {ok, Pid} ->
            mlock(Pid, Rest, Opts);
        {error, already_locked} ->
            ok = ezk:end_connection(Pid, mlock_failed),
            {error, {mlock_failed, Path}};
        {error, no_zk_connection} = Error ->
            Error
    end.

lock(Path) ->
    lock(Path, []).

lock(Path, Opts) ->
    Servers = application:get_env(zootils, zookeeper_servers, []),
    case ezk:start_connection(Servers) of
        {ok, Pid} ->
            lock(Pid, Path, Opts);
        {error, no_server_reached} ->
            {error, no_zk_connection}
    end.

lock(Pid, Path, Opts) ->
    case grab_lock(Pid, Path, Opts) of
        {ok, locked} ->
            {ok, Pid};
        {error, already_locked} ->
            {error, already_locked};
        {error, {no_zk_connection = ZKError, _}} ->
            {error, ZKError}
    end.

grab_lock(Pid, Path, Opts)
  when is_pid(Pid) andalso is_list(Path) andalso
       is_list(Opts) ->
    NodeBin = atom_to_binary(node(), latin1),
    ShouldCreateFullPath = proplists:get_value(create_full_paths, Opts),
    case ezk:create(Pid, Path, NodeBin, e) of
        {ok, Path} ->
            {ok, locked};
        {error, no_dir} = E ->
            case ShouldCreateFullPath of
                true ->
                    ok = create_parent_paths(Pid, Path),
                    grab_lock(Pid, Path, Opts);
                false ->
                    E
            end;
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

create_parent_paths(Pid, Path) ->
    FolderList = droplast(string:tokens(Path, "/")),
    CreateAsWeGo =
        fun(P, UpTo) ->
                NewFull = UpTo ++ "/" ++  P,
                ok = did_create(ezk:create(Pid, NewFull, <<>>)),
                NewFull
        end,
    lists:foldl(CreateAsWeGo, "", FolderList),
    ok.

droplast([_T])  -> [];
droplast([H|T]) -> [H|droplast(T)].

did_create({ok, _}) ->
    ok;
did_create({error, dir_exists}) ->
    ok.
