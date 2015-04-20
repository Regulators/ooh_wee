-module(ooh_wee_task_launcher).

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          path    :: string(),
          mod     :: atom(),
          zkconn  :: pid()
         }).


start_link(Path, Mod) ->
    NameAtom = list_to_atom(Path),
    gen_server:start_link({local, NameAtom}, ?MODULE, [Path, Mod], []).

init([Path, Mod]) ->
    do_init(Path, Mod).

do_init(Path, Mod) ->
    Servers = application:get_env(ooh_wee, zookeeper_servers, []),
    case ezk:start_connection(Servers) of
        {ok, Pid} ->
            {ok, Tasks} = ezk:ls(Pid, Path, self(), incoming_tasks),
            [start_module_for_child(Pid, Task, Mod) || Task <- Tasks],
            {ok, #state{zkconn=Pid, path=Path, mod=Mod}};
        {error, _} = E ->
            E
    end.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({incoming_tasks, WatchMessage}, #state{zkconn=Pid, path=Path, mod=Mod}=State) ->
    io:format("~p~n", [WatchMessage]),
    {ok, Tasks} = ezk:ls(Pid, Path, self(), incoming_tasks),
    [start_module_for_child(Pid, Task, Mod) || Task <- Tasks],
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

start_module_for_child(Pid, Task, Mod) ->
    TaskPath = task_path(Task),
    zk_utils:is_ok_create(ezk:create(Pid, "/ooh_wee/assigned", <<>>)),
    case ezk:create(Pid, TaskPath, atom_to_binary(Mod, latin1)) of
        {ok, _} ->
            spawn(fun() ->
                          %% This should probably make the connection itself
                          %% and make an ephemeral node.
                          %% TODO: Re-read ZK recipes
                          ModState = Mod:init(Task),
                          Mod:handle_task(ModState)
                  end),
            ok;
        {error, dir_exists} ->
            ok
    end.

task_path(Task) when is_binary(Task) ->
    %% TODO: Make the constituent paths in the supervisor
    list_to_binary(["/ooh_wee/assigned/", Task]).
