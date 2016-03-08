%%% @copyright 2016+ Hippware, Inc.
%%% @doc Wocky REST server configuration
%%%
%%% This module provides the RESTful HTTP server for wocky
-module(wocky_rest).

%%%===================================================================
%%% Top-level interface
%%%===================================================================

-export([start/1, stop/0, map_keys_to_atoms/1]).

-define(DEFAULT_PORT, 1096).

start(Opts) ->
    Port = proplists:get_value(port, Opts, ?DEFAULT_PORT),

    {ok, _} = application:ensure_all_started(webmachine),
    webmachine_mochiweb:start(
      [{name, wocky_reg},
       {port, Port},
       {dispatch, [
                   {["wocky", "v1", "user"], wocky_rest_reg, Opts},
                   {["wocky", "v1", "db", operation], wocky_rest_db, Opts}
                  ]}]),
    ok.

stop() ->
    ok = webmachine_mochiweb:stop(wocky_reg_mochiweb),
    ok.

map_keys_to_atoms(Map) ->
    lists:foldl(fun(K, M) -> binary_key_to_atom(K, M) end,
                Map, maps:keys(Map)).

binary_key_to_atom(Key, Map) ->
    {ok, Val} = maps:find(Key, Map),
    maybe_add_as_atom(Key, Val, (maps:remove(Key, Map))).

maybe_add_as_atom(Key, Val, Map) ->
    try list_to_existing_atom(binary_to_list(Key)) of
        AtomKey -> Map#{AtomKey => Val}
    catch
        error:badarg -> Map
    end.
