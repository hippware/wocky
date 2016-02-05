%%% @copyright 2016+ Hippware, Inc.
%%% @doc Helper functions for Wocky integration test suites
-module(test_helper).

-export([start_ejabberd/0, stop_ejabberd/0]).

start_ejabberd() ->
    case net_kernel:start(['mongooseim@localhost', longnames]) of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok
    end,

    wocky_app:start(),

    application:load(ejabberd),
    ConfigPath = "../../../../etc/ejabberd.cfg",
    application:set_env(ejabberd, config, ConfigPath),
    application:ensure_all_started(ejabberd),
    ok.

stop_ejabberd() ->
    application:stop(ejabberd),
    wocky_app:stop(),
    ok.
