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

-spec start(list()) -> ok.
start(Opts) ->
    Port = proplists:get_value(port, Opts, ?DEFAULT_PORT),

    {ok, _} = application:ensure_all_started(webmachine),
    webmachine_mochiweb:start([{name, wocky_api},
                               {port, Port},
                               {dispatch, routes(Opts)}]),
    ok.

-spec stop() -> ok.
stop() ->
    ok = webmachine_mochiweb:stop(wocky_api_mochiweb),
    ok.

-spec map_keys_to_atoms(map()) -> map().
map_keys_to_atoms(Map) ->
    lists:foldl(fun(K, M) -> binary_key_to_atom(K, M) end,
                Map, maps:keys(Map)).

binary_key_to_atom(Key, Map) ->
    {ok, Val} = maps:find(Key, Map),
    maybe_add_as_atom(Key, Val, (maps:remove(Key, Map))).

maybe_add_as_atom(Key, Val, Map) ->
    try binary_to_existing_atom(Key, utf8) of
        AtomKey -> Map#{AtomKey => Val}
    catch
        error:badarg -> Map
    end.

routes(Opts) ->
    Routes = [{["wocky", "v1", "user"], wocky_rest_reg, Opts}],
    maybe_add_testing_routes(wocky_app:is_testing(), Routes, Opts).

maybe_add_testing_routes(true, Routes, Opts) ->
    [{["wocky", "v1", "db", operation], wocky_rest_db, Opts}|Routes];
maybe_add_testing_routes(false, Routes, _Opts) ->
    Routes.
