%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite for mod_wocky_cli
-module(cli_SUITE).
-compile(export_all).
-compile({parse_transform, fun_chain}).

-include_lib("common_test/include/ct.hrl").

-include("wocky_db_seed.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() -> [befriend].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),
    wocky_db:clear_user_tables(?LOCAL_CONTEXT),
    Users = escalus:get_users([alice, bob, carol]),
    wocky_db_seed:seed_tables(shared, [handle_to_user]),
    fun_chain:first(Config,
        escalus:init_per_suite(),
        escalus:create_users(Users)
    ).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

befriend(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}],
      fun(Alice, Bob, Carol) ->
        {ok, _} = mod_wocky_cli:befriend(<<"alice">>, <<"bob">>),
        escalus:assert(is_roster_set, escalus:wait_for_stanza(Alice)),
        escalus:assert(is_roster_set, escalus:wait_for_stanza(Bob)),

        test_helper:ensure_all_clean([Alice, Bob, Carol])
      end).
