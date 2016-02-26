%%% @copyright 2016+ Hippware, Inc.
%%% @doc Wocky user registration server gen_mod module
%%%
%%% This module serves as a trivial gen_mod interface to start up the
%%% wocky_reg HTTP server.
-module(mod_wocky_reg).

-behaviour(gen_mod).

-export([start/2, stop/1]).

start(Host, Opts) ->
    wocky_reg:start([{server, Host} | Opts]).

stop(_Host) ->
    wocky_reg:stop().
