%%% @copyright 2015+ Hippware, Inc.
%%% @doc Cassandra configuration from ejabberd.cfg
%%%
%%% This module configures the Cassandra backend module using settings found in
%%% ejabberd.cfg. This is implemented as an ejabberd module.
%%% To use it, add this to the modules list in ejabberd.cfg (preferably first):
%%%
%%% `{mod_cassandra, []},'
%%%
%%% Other values can be added to the module options list. They are
%%% backend-specific and will be passed as-is to the backend module.
%%%
-module(mod_wocky).

%% gen_mod
-behaviour(gen_mod).
-export([start/2, stop/1]).


start(Host, _Opts) ->
    Config = ejabberd_config:get_local_option({cassandra_backend:backend(), Host}),
    wocky_db:configure(Host, Config).

stop(_Host) ->
    ok.
