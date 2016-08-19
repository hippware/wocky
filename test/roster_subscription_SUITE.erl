%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite for mod_roster_subscription
-module(roster_subscription_SUITE).
-compile(export_all).
-compile({parse_transform, fun_chain}).

-include("wocky.hrl").
-include("wocky_db_seed.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-import(test_helper, [expect_iq_success_u/3]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [subscribe,
     unsubscribe,
     get_roster,
     push_notifications
    ].

suite() ->
    escalus:suite().


%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),
    wocky_db_seed:clear_user_tables(?LOCAL_CONTEXT),
    wocky_db_seed:clear_tables(?LOCAL_CONTEXT, [roster]),
    wocky_db_seed:seed_tables(?LOCAL_CONTEXT, [roster]),
    Users = escalus:get_users([alice, bob, carol, karen]),
    fun_chain:first(Config,
        escalus:init_per_suite(),
        escalus:create_users(Users)
    ).

end_per_suite(Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob, carol, karen])),
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% mod_roster_subscription tests
%%--------------------------------------------------------------------

subscribe(Config) ->
    escalus:story(Config, [{alice, 1}, {carol, 1}],
      fun(Alice, Carol) ->
        % Successfully subscribe to a user's roster
        expect_iq_success_u(subscribe_stanza(), Carol, Alice),

        % Multiple subscriptions should still succeed
        expect_iq_success_u(subscribe_stanza(), Carol, Alice)
      end).

unsubscribe(Config) ->
    escalus:story(Config, [{alice, 1}, {carol, 1}, {karen, 1}],
      fun(Alice, Carol, Karen) ->
        % Successfully unsubscribe from a user's roster
        expect_iq_success_u(unsubscribe_stanza(), Carol, Alice),

        % Multiple unsubscriptions should still succeed
        expect_iq_success_u(unsubscribe_stanza(), Carol, Alice),

        % Unsubscriptions from non-subscribers should still succeed
        expect_iq_success_u(unsubscribe_stanza(), Karen, Alice)
      end).

get_roster(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        Stanza = expect_iq_success_u(get_roster_stanza(), Bob, Alice),
        check_returned_roster(
          Stanza, length(wocky_db_seed:seed_data(roster, ?LOCAL_CONTEXT)))
      end).

push_notifications(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}],
      fun(Alice, Bob, Carol) ->
        %% Bob subscribers to Alice's roster
        expect_iq_success_u(subscribe_stanza(), Bob, Alice),

        %% Alice removes a contact
        escalus:send(Alice, escalus_stanza:roster_remove_contact(Carol)),
        escalus:assert_many([is_roster_set, is_iq_result],
                            escalus:wait_for_stanzas(Alice, 2)),

        %% Bob gets an update
        Stanza = escalus:wait_for_stanza(Bob),
        check_deleted_roster_update(Stanza),

        %% Alice adds a contact
        escalus:send(Alice, escalus_stanza:roster_add_contact(
                              Carol, [<<"friends">>], <<"Cazza">>)),
        escalus:assert_many([is_roster_set, is_iq_result],
                            escalus:wait_for_stanzas(Alice, 2)),

        %% Bob gets an update
        Stanza2 = escalus:wait_for_stanza(Bob),
        check_returned_roster(Stanza2, 1)
      end).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

subscribe_stanza() ->
    subscription_management_stanza(set, <<"subscribe">>).

unsubscribe_stanza() ->
    subscription_management_stanza(set, <<"unsubscribe">>).

get_roster_stanza() ->
    subscription_management_stanza(get, <<"query">>).

subscription_management_stanza(Type, Name) ->
    IQ = #iq{type = Type, id = wocky_util:iq_id(),
             sub_el = #xmlel{name = Name,
                             attrs = [{<<"xmlns">>, ?NS_WOCKY_ROSTER}]}},
    jlib:iq_to_xml(IQ).

check_returned_roster(Stanza, ExpectedCount) ->
    Query = exml_query:subelement(Stanza, <<"query">>),
    check_roster_items(Query#xmlel.children, ExpectedCount).

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
