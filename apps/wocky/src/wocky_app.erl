-module(wocky_app).
-behaviour(application).

-export([start/0]).
-export([start/2, stop/1]).


start() ->
    application:start(wocky),
    ejabberd:start().

start(_StartType, _StartArgs) ->
    ok.

stop(_State) ->
    ok.
