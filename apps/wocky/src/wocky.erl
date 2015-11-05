-module(wocky).


%% API
-export([start/0]).

start() ->
    application:start(wocky),
    ejabberd:start().
