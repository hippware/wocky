%%% @copyright 2016+ Hippware, Inc.
%%% @doc Helper functions for Wocky integration test suites
-module(test_helper).

-export([start_ejabberd/0, stop_ejabberd/0]).

start_ejabberd() ->
    case net_kernel:start(['mongooseim@localhost', longnames]) of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok
    end,

    ok = wocky_app:start("test"),
    wocky_app:start_ejabberd("../../../../etc").

stop_ejabberd() ->
    wocky_app:stop().
