-module(ooh_wee_lock).

-export([lock/1]).
-export([unlock/2]).


lock(Path) ->
    Servers = application:get_env(ooh_wee, zookeeper_servers, []),
    case ezk:start_connection(Servers) of
        {ok, Pid} ->
            case grab_lock(Pid, Path) of
                {ok, locked} ->
                    {ok, locked};
                {error, already_locked} ->
                    {error, already_locked};
                {error, {no_zk_connection = ZKError, _}} ->
                    {error, ZKError}
            end;
        {error, no_server_reached} ->
            {error, no_zk_connection}
    end.

grab_lock(Pid, Path) ->
    case ezk:create(Pid, Path, <<>>, e) of
        {ok, Path} ->
            {ok, locked};
        {error, dir_exists} ->
            {error, already_locked}
    end.

unlock(Pid, Path) ->
    case ezk:delete(Pid, Path) of
        {ok, Path} ->
            {ok, unlocked};
        {error, no_dir} ->
            {ok, unlocked};
        {error, {no_zk_connection = ZKError, _}} ->
            {error, ZKError}
    end.
