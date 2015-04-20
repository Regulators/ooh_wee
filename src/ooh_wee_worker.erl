-module(ooh_wee_worker).


-callback init(Task :: term()) ->
    {ok, State :: term()}.

-callback handle_task(State :: term()) ->
    {ok, TaskData :: binary()}.
