%%%----------------------------------------------------------------------
%%% File    : wocky_debug.erl
%%% Author  : Beng Tan
%%% Purpose : Debugging utilities
%%% Copyright (c) 2015 Hippware
%%%
%%%----------------------------------------------------------------------

-module(wocky_debug).

-include("ejabberd.hrl").
-include("ejabberd_commands.hrl").

-export([poke/0, poke/1, poke/2]).

%% gen_mod
-behaviour(gen_mod).
-export([start/2, stop/1]).

%% gen_mod callbacks
start(_Host, _Opts) ->
    ejabberd_commands:register_commands(commands()).

stop(_Host) ->
    ejabberd_commands:unregister_commands(commands()).

%%%
%%% ejabberd commands
%%%

-spec commands() -> [ejabberd_commands:cmd()].
commands() ->
    [
     #ejabberd_commands{name = poke0, tags = [debug],
                        desc = "Do a poke (debugging)",
                        module = ?MODULE, function = poke,
                        args = [],
                        result = {res, restuple}},
     #ejabberd_commands{name = poke1, tags = [debug],
                        desc = "Do a poke (debugging)",
                        module = ?MODULE, function = poke,
                        args = [{arg1, binary}],
                        result = {res, restuple}},
     #ejabberd_commands{name = poke2, tags = [debug],
                        desc = "Do a poke (debugging)",
                        module = ?MODULE, function = poke,
                        args = [{arg1, binary}, {arg2, binary}],
                        result = {res, restuple}}
    ].

poke() ->
    poke(<<"">>, <<"">>).

poke(Arg1) ->
    poke(Arg1, <<"">>).

% To run arbitrary code, add the code here.
poke(Arg1, Arg2) ->
    Text = io_lib:format("Poke ~p ~p", [Arg1, Arg2]),
    {ok, Text}.
