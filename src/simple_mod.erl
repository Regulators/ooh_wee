-module(simple_mod).

-export([init/1]).
-export([handle_task/1]).

-behaviour(ooh_wee_worker).

init(Task) ->
    io:format("Starting tas: ~p~n", [Task]).

handle_task(_) ->
    io:format("handle_task~n", []).
