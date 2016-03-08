%%% @copyright 2016+ Hippware, Inc.
%%% @doc Wocky REST server configuration
%%%
%%% This module provides the RESTful HTTP server for wocky
-module(wocky_rest).

%%%===================================================================
%%% Top-level interface
%%%===================================================================

-export([start/1, stop/0]).

-define(DEFAULT_PORT, 1096).

start(Opts) ->
    Port = proplists:get_value(port, Opts, ?DEFAULT_PORT),

    {ok, _} = application:ensure_all_started(webmachine),
    webmachine_mochiweb:start(
      [{name, wocky_reg},
       {port, Port},
       {dispatch, [{["wocky", "v1", "user"], wocky_rest_reg, Opts}]}]),
    ok.

stop() ->
    ok = webmachine_mochiweb:stop(wocky_reg_mochiweb),
    ok.
