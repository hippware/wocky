%%% @copyright 2016+ Hippware, Inc.
%%% @doc Wocky REST server gen_mod module
%%%
%%% This module serves as a trivial gen_mod interface to start up the
%%% wocky_rest HTTP REST server.
-module(mod_wocky_rest).

-behaviour(gen_mod).

-export([start/2, stop/1]).

start(Host, Opts) ->
    wocky_rest:start([{server, Host} | Opts]).

stop(_Host) ->
    wocky_rest:stop().
