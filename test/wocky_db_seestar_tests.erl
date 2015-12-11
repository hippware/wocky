%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for cassandra_seestar.erl
-module(wocky_db_seestar_tests).

-include_lib("eunit/include/eunit.hrl").


wocky_db_seestar_configure_test_() -> {
  foreach,
  fun _Before() ->
    ok = wocky_app:start()
  end,
  fun _After(_) ->
    ok = wocky_app:stop()
  end,
  [
    { "configure/2 should start worker pools", [
      ?_assert(is_process_alive(whereis(wocky_db_shared_pool))),
      ?_assert(is_process_alive(whereis(wocky_db_localhost_pool)))
    ]},
    { "clear/0 should stop worker pools", [
      ?_test(
        begin
          wocky_db_seestar:clear(),
          ?assertEqual(undefined, whereis(wocky_db_shared_pool)),
          ?assertEqual(undefined, whereis(wocky_db_localhost_pool))
        end)
    ]}
  ]
}.
