%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite for mod_roster_query
-module(roster_query_SUITE).
-compile(export_all).
-compile({parse_transform, fun_chain}).

-include("wocky.hrl").
-include("wocky_db_seed.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-import(test_helper, [expect_iq_success_u/3, expect_iq_error_u/3]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     get_roster,
     invalid_query,
     roster_change_notify,
     permissions
    ].

suite() ->
    escalus:suite().


%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),
    wocky_db:clear_user_tables(?LOCAL_CONTEXT),
    wocky_db_seed:seed_tables(shared, [roster]),
    Users = escalus:get_users([alice, bob, carol, karen]),
    fun_chain:first(Config,
        escalus:init_per_suite(),
        escalus:create_users(Users)
    ).

end_per_suite(Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob, carol, karen])),
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
    ok = wocky_db:update(shared, user,
                         #{roster_viewers => [?BOB_B_JID]},
                         #{user => ?ALICE, server => ?LOCAL_CONTEXT}),
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% mod_roster_subscription tests
%%--------------------------------------------------------------------

get_roster(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->

        %% No specified version returns the full roster
        Stanza = expect_iq_success_u(get_roster_stanza(undefined), Bob, Alice),
        Version = check_returned_roster(Stanza, 4),

        %% Specifying the current version returns no items
        Stanza2 = expect_iq_success_u(get_roster_stanza(Version), Bob, Alice),
        Version = check_returned_roster(Stanza2, 0),

        %% Specifying a non-current version returns all items
        Stanza3 = expect_iq_success_u(get_roster_stanza(<<"wrong_ver">>),
                                      Bob, Alice),
        Version = check_returned_roster(Stanza3, 4)
      end).

invalid_query(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        %% Invalid query type returns an error
        expect_iq_error_u(roster_stanza(undefined, <<"blah">>), Bob, Alice)
      end).

roster_change_notify(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}, {karen, 1}],
      fun(Alice, Bob, Carol, Karen) ->
         %% Alice removes a contact
         escalus:send(Alice, escalus_stanza:roster_remove_contact(Karen)),
         escalus:assert_many([is_roster_set, is_iq_result],
                             escalus:wait_for_stanzas(Alice, 2)),

         %% Bob gets an update
         Stanza = escalus:wait_for_stanza(Bob),
         check_roster_update(Stanza),

         %% Alice adds a contact
         escalus:send(Alice, escalus_stanza:roster_add_contact(
                               Karen, [<<"friends">>], <<"Cazza">>)),
         escalus:assert_many([is_roster_set, is_iq_result],
                             escalus:wait_for_stanzas(Alice, 2)),

         %% Bob gets an update
         Stanza2 = escalus:wait_for_stanza(Bob),
         check_roster_update(Stanza2),

         %% Carol should not have received anything, not being a viewer
         test_helper:ensure_all_clean([Alice, Bob, Carol])
      end).

permissions(Config) ->
    escalus:story(Config, [{alice, 1}, {carol, 1}],
      fun(Alice, Carol) ->
        %% Unlike Bob, Carol can't get the roster because she's not in
        %% Alice's roster_viewers list
        expect_iq_error_u(get_roster_stanza(undefined), Carol, Alice)
      end).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

get_roster_stanza(Version) ->
    roster_stanza(Version, <<"query">>).

roster_stanza(Version, Name) ->
    IQ = #iq{type = get, id = wocky_util:iq_id(),
             sub_el = #xmlel{name = Name,
                             attrs = [{<<"xmlns">>, ?NS_WOCKY_ROSTER} |
                                      maybe_ver_attr(Version)]}},
    jlib:iq_to_xml(IQ).

maybe_ver_attr(undefined) -> [];
maybe_ver_attr(Version) -> [{<<"version">>, Version}].

check_returned_roster(Stanza, ExpectedCount) ->
    Query = exml_query:subelement(Stanza, <<"query">>),
    check_roster_items(Query#xmlel.children, ExpectedCount),
    {value, Version} = xml:get_attr(<<"version">>, Query#xmlel.attrs),
    Version.

check_roster_items(Items, ExpectedCount) ->
    lists:foreach(fun check_roster_item/1, Items),
    ?assertEqual(ExpectedCount, length(Items)).

check_roster_item(#xmlel{name = <<"item">>, attrs = Attrs,
                         children = Children}) ->
    ?assertMatch({value, _}, xml:get_attr(<<"subscription">>, Attrs)),
    ?assertEqual([#xmlel{name = <<"group">>,
                         children = [#xmlcdata{content = <<"friends">>}]}],
                 Children),
    check_roster_item_common(Attrs).

check_deleted_roster_update(Stanza) ->
    Query = exml_query:subelement(Stanza, <<"query">>),
    check_deleted_roster_item(hd(Query#xmlel.children)).

check_deleted_roster_item(#xmlel{name = <<"item">>, attrs = Attrs}) ->
    ?assertMatch(false, xml:get_attr(<<"subscription">>, Attrs)),
    check_roster_item_common(Attrs).

check_roster_item_common(Attrs) ->
    ?assertMatch({value, _}, xml:get_attr(<<"jid">>, Attrs)),
    ?assertMatch({value, _}, xml:get_attr(<<"name">>, Attrs)).

check_roster_update(Stanza = #xmlel{attrs = Attrs, children = [Child]}) ->
    escalus:assert(is_message, Stanza),
    ?assertEqual({value, <<"headline">>}, xml:get_attr(<<"type">>, Attrs)),
    ?assertMatch(#xmlel{name = <<"roster-changed">>}, Child).
