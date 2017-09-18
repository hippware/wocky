%%% @copyright 2017+ Hippware, Inc.
%%% @doc Integration test suite for blocking functionality
-module(blocking_SUITE).

-compile(export_all).
-compile({parse_transform, fun_chain}).
-compile({parse_transform, cut}).
-compile({parse_transform, do}).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("test_helper.hrl").

-import(test_helper, [expect_iq_error/2, expect_iq_success/2, subscribe_pair/2,
                      remove_friend/2, add_to_s/2, iq_get/2,
                      publish_item_stanza/4]).
-import(user_SUITE, [get_request/2, users_request/1, expect_bulk_results/2,
                     contact_request/3, check_returned_contacts/3,
                     public_fields/0]).
-import(bot_SUITE, [retrieve_stanza/1, retrieve_stanza/2,
                    explore_nearby_stanza/2, expect_explore_result/2,
                    item_query_el/2, check_returned_bots/4]).
-import(conversation_SUITE, [query_stanza/4, check_ret/4]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     block,
     {group, blocked},
     unblock,
     {group, unblocked}
    ].

groups() ->
    [
     {blocked, [], [
                    blocked_roster_access,
                    blocked_user_access,
                    blocked_bot_access,
                    blocked_bot_by_user_access,
                    blocked_publish_access,
                    blocked_explore_access,
                    blocked_item_access,
                    blocked_contact_access,
                    blocked_conversation_access,
                    blocked_messages
                   ]},
     {unblocked, [], [
                    unblocked_roster_access,
                    unblocked_user_access,
                    unblocked_bot_access,
                    unblocked_bot_by_user_access,
                    unblocked_publish_access,
                    unblocked_explore_access,
                    unblocked_item_access,
                    unblocked_messages,
                    unblocked_contact_access
                   ]}
    ].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),

    Config2 =
    fun_chain:first(
      Config,
      escalus:init_per_suite(),
      test_helper:setup_users([alice, bob, carol])
     ),

    Alice = ?wocky_repo:get(?wocky_user, ?ALICE),
    Bob = ?wocky_repo:get(?wocky_user, ?BOB),
    Carol = ?wocky_repo:get(?wocky_user, ?CAROL),
    ?wocky_user:update(?ALICE, #{handle => <<"Alice">>}),
    ?wocky_user:update(?BOB, #{handle => <<"Bob">>}),
    ?wocky_user:update(?CAROL, #{handle => <<"Carol">>}),

    AlicesBot = ?wocky_factory:insert(
                   bot, #{user => Alice, public => true,
                          location => ?wocky_geo_utils:point(1.0, 1.0)}),
    BobsBot = ?wocky_factory:insert(
                 bot, #{user => Bob, public => true,
                        location => ?wocky_geo_utils:point(2.0, 2.0)}),
    CarolsBot = ?wocky_factory:insert(
                   bot, #{user => Carol, public => true,
                          location => ?wocky_geo_utils:point(3.0, 3.0)}),

    ?wocky_factory:insert(item, #{user => Alice, bot => CarolsBot}),
    ?wocky_factory:insert(item, #{user => Bob, bot => CarolsBot}),
    ?wocky_factory:insert(item, #{user => Carol, bot => CarolsBot}),

    ?wocky_factory:insert(conversation,
                          #{user => Alice, other_jid => ?BJID(?BOB)}),
    ?wocky_factory:insert(conversation,
                          #{user => Bob, other_jid => ?BJID(?ALICE)}),

    [{alices_bot, AlicesBot},
     {bobs_bot, BobsBot},
     {carols_bot, CarolsBot}
     | Config2].

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).
%%--------------------------------------------------------------------
%% blocking tests
%%--------------------------------------------------------------------

block(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        SetStanza = escalus_stanza:roster_add_contact(
                      Bob, [<<"__blocked__">>], <<"GoAway">>),
        escalus:send(Alice, SetStanza),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),

        BobPush = escalus:wait_for_stanza(Bob),
        escalus:assert(is_roster_set, BobPush),
        escalus_client:send(Bob, escalus_stanza:iq_result(BobPush)),

        ?assert(has_no_groups(BobPush))
      end).

blocked_roster_access(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        % Alice can see Bob, since she blocked him. He will have the __blocked__
        % group.
        escalus_client:send(Alice, escalus_stanza:roster_get()),
        AliceResult = escalus:wait_for_stanza(Alice),
        escalus_assert:is_roster_result(AliceResult),
        escalus_assert:count_roster_items(1, AliceResult),
        ?assert(has_group(AliceResult, ?wocky_blocking:blocked_group())),

        % Bob can't see Alice since she blocked him. Any attempt to create or
        % change a roster entry for Alice will fail.
        escalus_client:send(Bob, escalus_stanza:roster_get()),
        BobResult = escalus:wait_for_stanza(Bob),
        escalus_assert:is_roster_result(BobResult),
        escalus_assert:count_roster_items(0, BobResult),

        AddStanza = escalus_stanza:roster_add_contact(Alice, [<<"frienemies">>],
                                                      <<"Please like me">>),
        expect_iq_error(AddStanza, Bob)
      end).

blocked_user_access(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        % Single user requests
        AliceGetReq = get_request(?BOB, []),
        expect_iq_error(AliceGetReq, Alice),

        BobGetReq = get_request(?ALICE, []),
        expect_iq_error(BobGetReq, Bob),

        % Bulk user requests
        AliceBulkReq = users_request([?BJID(?BOB)]),
        #xmlel{children = [Result]} = expect_iq_success(AliceBulkReq, Alice),
        expect_bulk_results(Result, [{?BJID(?BOB), error}]),

        BobBulkReq = users_request([?BJID(?ALICE)]),
        #xmlel{children = [Result2]} = expect_iq_success(BobBulkReq, Bob),
        expect_bulk_results(Result2, [{?BJID(?ALICE), error}])
      end).

blocked_bot_access(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        Stanza = retrieve_stanza(
                   maps:get(id, proplists:get_value(bobs_bot, Config))),
        expect_iq_error(Stanza, Alice),

        Stanza2 = retrieve_stanza(
                    maps:get(id, proplists:get_value(alices_bot, Config))),
        expect_iq_error(Stanza2, Bob)
      end).

blocked_bot_by_user_access(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        Stanza = expect_iq_success(
                   retrieve_stanza(?BJID(?BOB), #rsm_in{}), Alice),
        check_returned_bots(Stanza, [], undefined, 0),

        Stanza2 = expect_iq_success(
                   retrieve_stanza(?BJID(?ALICE), #rsm_in{}), Bob),
        check_returned_bots(Stanza2, [], undefined, 0)
      end).

blocked_publish_access(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        expect_iq_error(
          publish_item_stanza(
            maps:get(id, proplists:get_value(bobs_bot, Config)),
            <<"blockednote">>, <<"title">>, <<"content">>),
        Alice),

        expect_iq_error(
          publish_item_stanza(
            maps:get(id, proplists:get_value(alices_bot, Config)),
            <<"blockednote">>, <<"title">>, <<"content">>),
        Bob)
      end).

blocked_explore_access(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        % Explore-nearby should find their own bots, but not ones of
        % blocked users
        expect_iq_success(add_to_s(explore_nearby_stanza(100000000.0, 100),
                                   Alice), Alice),
        expect_explore_result(proplists:get_value(alices_bot, Config), Alice),
        expect_explore_result(<<"no-more-bots">>, Alice),

        expect_iq_success(add_to_s(explore_nearby_stanza(100000000.0, 100),
                                   Bob), Bob),
        expect_explore_result(proplists:get_value(bobs_bot, Config), Bob),
        expect_explore_result(<<"no-more-bots">>, Bob)
      end).

blocked_item_access(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        BotID = maps:get(id, proplists:get_value(carols_bot, Config)),
        Request = iq_get(?NS_BOT, item_query_el(#rsm_in{}, BotID)),
        Result = expect_iq_success(Request, Alice),
        expect_items_not_owner(Result, 2, ?BOB),

        Request2 = iq_get(?NS_BOT, item_query_el(#rsm_in{}, BotID)),
        Result2 = expect_iq_success(Request2, Bob),
        expect_items_not_owner(Result2, 2, ?ALICE)
      end).

blocked_contact_access(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}],
      fun(Alice, Bob, Carol) ->
        subscribe_pair(Bob, Carol),
        % Alice asks carol for their followers which should exclude bob
        lists:foreach(
          fun(Type) ->
            fun_chain:first(
              ?CAROL,
              contact_request(atom_to_binary(Type, utf8), #rsm_in{}),
              expect_iq_success(Alice),
              check_returned_contacts([{Type, []}], [Type])
             )
          end,
        [friend, follower, following]),

        remove_friend(Bob, Carol)
      end).

blocked_conversation_access(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        Stanza = query_stanza(undefined, undefined, undefined, undefined),
        Result = test_helper:expect_iq_success_u(Stanza, Alice),
        check_ret(Result, 0, 0, undefined),

        Stanza2 = query_stanza(undefined, undefined, undefined, undefined),
        Result2 = test_helper:expect_iq_success_u(Stanza2, Bob),
        check_ret(Result2, 0, 0, undefined)
      end).

blocked_messages(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi Bob!">>)),
        escalus:assert(is_error, [<<"cancel">>, <<"service-unavailable">>],
                       escalus:wait_for_stanza(Alice)),
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi Alice!">>)),
        escalus:assert(is_error, [<<"cancel">>, <<"service-unavailable">>],
                       escalus:wait_for_stanza(Bob)),

        timer:sleep(400),

        escalus_assert:has_no_stanzas(Alice),
        escalus_assert:has_no_stanzas(Bob)
      end).

%%--------------------------------------------------------------------
%% unblocking tests
%%--------------------------------------------------------------------


unblock(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        SetStanza = escalus_stanza:roster_add_contact(
                      Bob, [], <<"Fine, come back.">>),
        escalus:send(Alice, SetStanza),
        escalus:assert_many([is_roster_set, is_iq_result],
                            escalus:wait_for_stanzas(Alice, 2)),

        subscribe_pair(Alice, Bob)
      end).

unblocked_roster_access(Config) ->
    escalus:story([everyone_is_friends | Config], [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        escalus_client:send(Alice, escalus_stanza:roster_get()),
        AliceResult = escalus:wait_for_stanza(Alice),
        escalus_assert:is_roster_result(AliceResult),
        escalus_assert:count_roster_items(1, AliceResult),
        ?assert(has_no_groups(AliceResult)),

        escalus_client:send(Bob, escalus_stanza:roster_get()),
        BobResult = escalus:wait_for_stanza(Bob),
        escalus_assert:is_roster_result(BobResult),
        escalus_assert:count_roster_items(1, BobResult),
        ct:log("BobResult: ~p", [BobResult]),
        ?assert(has_group(BobResult, <<"__new__">>)),

        AddStanza = escalus_stanza:roster_add_contact(Alice, [<<"frienemies">>],
                                                      <<"welcome back">>),
        escalus:send(Bob, AddStanza),
        Received = escalus:wait_for_stanzas(Bob, 2),
        escalus:assert_many([is_roster_set, is_iq_result], Received)
      end).

unblocked_user_access(Config) ->
    escalus:story([everyone_is_friends | Config], [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        % Single user requests
        AliceGetReq = get_request(?BOB, []),
        expect_iq_success(AliceGetReq, Alice),

        BobGetReq = get_request(?ALICE, []),
        expect_iq_success(BobGetReq, Bob),

        % Bulk user requests
        AliceBulkReq = users_request([?BJID(?BOB)]),
        #xmlel{children = [Result]} = expect_iq_success(AliceBulkReq, Alice),
        expect_bulk_results(Result, [{?BJID(?BOB), public_fields()}]),

        BobBulkReq = users_request([?BJID(?ALICE)]),
        #xmlel{children = [Result2]} = expect_iq_success(BobBulkReq, Bob),
        expect_bulk_results(Result2, [{?BJID(?ALICE), public_fields()}])
      end).

unblocked_bot_access(Config) ->
    escalus:story([everyone_is_friends | Config], [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        Stanza = retrieve_stanza(
                   maps:get(id, proplists:get_value(bobs_bot, Config))),
        expect_iq_success(Stanza, Alice),

        Stanza2 = retrieve_stanza(
                    maps:get(id, proplists:get_value(alices_bot, Config))),
        expect_iq_success(Stanza2, Bob)
      end).

unblocked_bot_by_user_access(Config) ->
    escalus:story([everyone_is_friends | Config], [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        Stanza = expect_iq_success(
                   retrieve_stanza(?BJID(?BOB), #rsm_in{}), Alice),
        check_returned_bots(
          Stanza, [maps:get(id, proplists:get_value(bobs_bot, Config))],
          0, 1),

        Stanza2 = expect_iq_success(
                   retrieve_stanza(?BJID(?ALICE), #rsm_in{}), Bob),
        check_returned_bots(
          Stanza2, [maps:get(id, proplists:get_value(alices_bot, Config))],
          0, 1)
      end).

unblocked_publish_access(Config) ->
    escalus:story([everyone_is_friends | Config], [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        expect_iq_success(
          publish_item_stanza(
            maps:get(id, proplists:get_value(bobs_bot, Config)),
            <<"blockednote">>, <<"title">>, <<"content">>),
        Alice),

        expect_iq_success(
          publish_item_stanza(
            maps:get(id, proplists:get_value(alices_bot, Config)),
            <<"blockednote">>, <<"title">>, <<"content">>),
        Bob)
      end).

unblocked_explore_access(Config) ->
    escalus:story([everyone_is_friends | Config], [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        % Explore-nearby should find their own bots, and those of their friends
        expect_iq_success(add_to_s(explore_nearby_stanza(100000000.0, 100),
                                   Alice), Alice),
        expect_explore_result(proplists:get_value(alices_bot, Config), Alice),
        expect_explore_result(proplists:get_value(bobs_bot, Config), Alice),
        expect_explore_result(<<"no-more-bots">>, Alice),

        expect_iq_success(add_to_s(explore_nearby_stanza(100000000.0, 100),
                                   Bob), Bob),
        expect_explore_result(proplists:get_value(alices_bot, Config), Bob),
        expect_explore_result(proplists:get_value(bobs_bot, Config), Bob),
        expect_explore_result(<<"no-more-bots">>, Bob)
      end).

unblocked_item_access(Config) ->
    escalus:story([everyone_is_friends | Config], [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        BotID = maps:get(id, proplists:get_value(carols_bot, Config)),
        Request = iq_get(?NS_BOT, item_query_el(#rsm_in{}, BotID)),
        Result = expect_iq_success(Request, Alice),
        expect_items_not_owner(Result, 3, ?TIM),

        Request2 = iq_get(?NS_BOT, item_query_el(#rsm_in{}, BotID)),
        Result2 = expect_iq_success(Request2, Bob),
        expect_items_not_owner(Result2, 3, ?TIM)
      end).

unblocked_messages(Config) ->
    escalus:story([everyone_is_friends | Config], [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi B!">>)),
        escalus_client:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi A!">>)),

        escalus_assert:is_chat_message(
          <<"Hi A!">>, escalus_client:wait_for_stanza(Alice)),
        escalus_assert:is_chat_message(
          <<"Hi B!">>, escalus_client:wait_for_stanza(Bob)),

        remove_friend(Alice, Bob)
      end).

unblocked_contact_access(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}],
      fun(Alice, Bob, Carol) ->
        subscribe_pair(Bob, Carol),
        BobUser = ?wocky_repo:get(?wocky_user, ?BOB),
        % Alice asks carol for their followers which should now include bob
        fun_chain:first(
          ?CAROL,
          contact_request(<<"friend">>, #rsm_in{}),
          expect_iq_success(Alice),
          check_returned_contacts([{friend, [BobUser]}], [friend])
         )
      end).

%%--------------------------------------------------------------------
%% helpers
%%--------------------------------------------------------------------

has_group(Stanza, Group) ->
    get_groups(Stanza) =:= [Group].

has_no_groups(Stanza) ->
    get_groups(Stanza) =:= [].

get_groups(Stanza) ->
    exml_query:paths(Stanza, [{element, <<"query">>},
                              {element, <<"item">>},
                              {element, <<"group">>},
                              cdata]).

expect_items_not_owner(Stanza, Count, NotOwner) ->
    Items = exml_query:paths(Stanza, [{element, <<"query">>},
                                      {element, <<"item">>}]),
    ?assertEqual(Count, length(Items)),
    NotOwnerJID = jid:to_binary(jid:make(NotOwner, ?SERVER, <<>>)),
    lists:foreach(
      fun(#xmlel{attrs = Attrs}) ->
              {value, Author} = xml:get_attr(<<"author">>, Attrs),
              ?assertNotEqual(Author, NotOwnerJID)
      end, Items).
