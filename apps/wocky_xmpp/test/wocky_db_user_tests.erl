%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for wocky_db_user.erl
-module(wocky_db_user_tests).

-include_lib("eunit/include/eunit.hrl").
-include("wocky_db_seed.hrl").

-import(wocky_db_user,
        [set_location/6]).

wocky_db_user_test_() ->
    {"wocky_db_user",
     setup, fun before_all/0, fun after_all/1, [
     test_set_location()
    ]}.

before_all() ->
    ok = wocky_db:clear_tables(?LOCAL_CONTEXT, [location]),
    ok.

after_all(_) ->
    ok.


test_set_location() ->
    CountMatch = #{user => ?ALICE, server => ?SERVER},
    { "set a user's location", [
      { "set a user's location without overwriting previous ones", [
        ?_assertEqual(ok, set_location(?ALICE, ?SERVER, ?RESOURCE, 1, 2, 3)),
        ?_assertEqual(1, wocky_db:count(?LOCAL_CONTEXT, location, CountMatch)),
        ?_assertEqual(ok, set_location(?ALICE, ?SERVER, ?RESOURCE,
                                       4.0, 5.0, 6.0)),
        ?_assertEqual(2, wocky_db:count(?LOCAL_CONTEXT, location, CountMatch)),
        ?_assertEqual(ok, set_location(?ALICE, ?SERVER, ?RESOURCE,
                                       6.6, 7.7, 8.8)),
        ?_assertEqual(3, wocky_db:count(?LOCAL_CONTEXT, location, CountMatch))
      ]},
      { "first location result should be most recently set location", [
        ?_assertEqual(#{lat => 6.6, lon => 7.7, accuracy => 8.8},
                      wocky_db:select_row(?LOCAL_CONTEXT, location,
                                          [lat, lon, accuracy],
                                          CountMatch))
      ]}
    ]}.
