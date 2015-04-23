-module(zk_utils).

-export([is_ok_create/1]).


is_ok_create({ok, _}) ->
    ok;
is_ok_create({error, dir_exists}) ->
    ok.
