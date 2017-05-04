%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite for MIM IQ handler crash handling
-module(iq_crash_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("test_helper.hrl").


%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [crash_iq].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),
    gen_mod:start_module(?SERVER, mod_wocky_crash_test, []),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    gen_mod:stop_module(?SERVER, mod_wocky_crash_test),
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
    Config1 = test_helper:setup_users(Config, [alice]),
    escalus:init_per_testcase(CaseName, Config1).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% MIM IQ crash handler tests
%%--------------------------------------------------------------------

crash_iq(Config) ->
    escalus:story(Config, [{alice, 1}],
      fun (Alice) ->
        ejabberd_config:add_local_option(iq_crash_response, error_with_dump),
        check_details(
          test_helper:expect_iq_error(crash_stanza(), Alice), true),
        check_details(
          test_helper:expect_iq_error_u(crash_stanza(), Alice, Alice), true),

        ejabberd_config:add_local_option(iq_crash_response, error),
        check_details(
          test_helper:expect_iq_error(crash_stanza(), Alice), false),
        check_details(
          test_helper:expect_iq_error_u(crash_stanza(), Alice, Alice), false)
      end).

crash_stanza() ->
    test_helper:iq_set(?NS_CRASH_TEST, #xmlel{name = <<"crash">>}).

check_details(Stanza, false) ->
    ?assertEqual(<<>>, xml:get_path_s(Stanza, error_path()));
check_details(Stanza, true) ->
    ?assertNotEqual(<<>>, xml:get_path_s(Stanza, error_path())).

error_path() ->
    [{elem, <<"error">>}, {elem, <<"text">>}, cdata].
