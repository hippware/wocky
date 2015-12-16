%%% @copyright 2015+ Hippware, Inc.
%%% @doc Debugging utilities
%%%
%%% Registers the "poke[0|1|2]" commands so it can be called from the command
%%% line to execute arbitrary code.

-module(mod_wocky_debug).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/ejabberd_commands.hrl").

-export([poke/0, poke/1, poke/2]).

%% gen_mod
-behaviour(gen_mod).
-export([start/2, stop/1]).

%% @doc Start the module
start(_Host, _Opts) ->
    ejabberd_commands:register_commands(commands()).

%% @doc Stop the module
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

%% @doc Call poke/2 with default arguments
poke() ->
    poke(<<"">>, <<"">>).

%% @doc Call poke/2 with default arguments
poke(Arg1) ->
    poke(Arg1, <<"">>).

%% @doc A placeholder function. To run arbitrary code, insert the code here.
poke(Arg1, Arg2) ->
    Text = io_lib:format("Poke ~p ~p", [Arg1, Arg2]),
    {ok, Text}.
