%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite for home stream
-module(home_stream_SUITE).

-compile(export_all).
-compile({parse_transform, fun_chain}).
-compile({parse_transform, cut}).
-compile({parse_transform, do}).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("test_helper.hrl").
-include("wocky_bot.hrl").

-import(test_helper, [expect_iq_success_u/3, expect_iq_error_u/3,
                      expect_iq_success/2, add_to_u/2,
                      ensure_all_clean/1, publish_item_stanza/4,
                      get_hs_stanza/0, get_hs_stanza/1,
                      check_hs_result/2, check_hs_result/4,
                      query_el/1, hs_node/1, node_el/3,
                      subscribe_stanza/0, check_home_stream_sizes/2,
                      check_home_stream_sizes/3]).

-define(NS_TEST, <<"test-item-ns">>).
-define(BOB_HS_ITEM_COUNT, 250).
-define(BOT_UPDATE_STANZA, <<"<bot_update>A thing happened</bot_update>">>).


%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() -> [
          get,
          get_first,
          get_with_rsm,
          publish,
          delete,
          no_auto_publish_chat,
          subscribe,
          subscribe_version,
          unsubscribe,
          maintain_subscription,
          get_item,
          {group, publish_bot},
          no_auto_publish_pep_item
         ].

groups() ->
    [{publish_bot, [], publish_bot_cases()}].

publish_bot_cases() ->
    [
     auto_publish_newly_public_bot,
     auto_publish_new_bot_friends,
     auto_publish_new_bot_non_friends,
     auto_publish_shared_private_bot,
     auto_publish_bot_item,
     auto_publish_to_system_user,
     bot_description_update,
     bot_becomes_private
    ].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

users() ->
    [alice, bob, carol, karen, tim].

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),
    fun_chain:first(Config,
        escalus:init_per_suite(),
        test_helper:setup_users(users()),
        test_helper:make_everyone_friends(escalus:get_users(users())),
        seed_home_stream()
    ).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(publish_bot, Config) ->
    User = ?wocky_repo:get(?wocky_user, ?ALICE),
    ?wocky_factory:insert(bot, #{id => ?BOT, user => User}),
    clear_home_streams(),
    seed_home_stream(Config);
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Test suites
%%--------------------------------------------------------------------

get(Config) ->
    escalus:story(Config, [{alice, 1}, {carol, 1}],
      fun(Alice, Carol) ->
        Stanza = expect_iq_success_u(get_hs_stanza(), Alice, Alice),
        check_hs_result(Stanza, 3),

        % Carol can't see Alice's home stream
        expect_iq_error_u(get_hs_stanza(), Carol, Alice),

        % Carol's own stream is empty
        Stanza2 = expect_iq_success_u(get_hs_stanza(), Carol, Carol),
        check_hs_result(Stanza2, 0, 0, false)
      end).

get_first(Config) ->
    escalus:story(Config, [{alice, 1}],
      fun(Alice) ->
        Stanza = expect_iq_success_u(
                   get_hs_stanza(#rsm_in{direction = before, max = 1}),
                   Alice, Alice),
        [Item] = check_hs_result(Stanza, 1),
        V3 = lists:last(proplists:get_value(alice_versions, Config)),
        ?assertEqual(V3, Item#item.version),
        test_helper:check_attr(<<"version">>, V3, Stanza)
      end).

get_with_rsm(Config) ->
    escalus:story(Config, [{alice, 1}],
      fun(Alice) ->
        [V1, _, V3] = proplists:get_value(alice_versions, Config),
        [_, K2, _] = proplists:get_value(alice_keys, Config),
        Stanza = expect_iq_success_u(
                   get_hs_stanza(#rsm_in{direction = before, max = 1, id = K2}),
                   Alice, Alice),
        [Item] = check_hs_result(Stanza, 1),
        ?assertEqual(V1, Item#item.version),
        test_helper:check_attr(<<"version">>, V3, Stanza)
      end).

publish(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        % Server can also auto-generate IDs if none is defined:
        expect_iq_success_u(pub_stanza(undefined), Alice, Alice),

        expect_iq_success_u(pub_stanza(<<"some_id">>), Alice, Alice),

        % Bob can't publish to Alice's home stream
        expect_iq_error_u(pub_stanza(<<"other_id">>), Bob, Alice),

        Stanza = expect_iq_success_u(get_hs_stanza(), Alice, Alice),
        check_hs_result(Stanza, 5)
      end).

delete(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        Stanza = expect_iq_success_u(get_hs_stanza(), Alice, Alice),
        check_hs_result(Stanza, 5),

        expect_iq_success_u(delete_stanza(), Alice, Alice),

        % Bob can't delete from Alice's home stream
        expect_iq_error_u(delete_stanza(), Bob, Alice),

        Stanza2 = expect_iq_success_u(get_hs_stanza(), Alice, Alice),
        Items = check_hs_result(Stanza2, 4, 1, true),
        ?assertEqual({delete, <<"some_id">>}, lists:last(Items)),

        Stanza3 = expect_iq_success_u(get_hs_stanza(true), Alice, Alice),
        check_hs_result(Stanza3, 4, 0, true)
      end).

no_auto_publish_chat(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        Stanza = escalus_stanza:chat_to(?BJID(?ALICE), <<"All your base">>),
        escalus:send(Bob, Stanza),
        escalus:assert(is_chat_message, escalus_client:wait_for_stanza(Alice)),

        Stanza2 = expect_iq_success_u(get_hs_stanza(), Alice, Alice),
        check_hs_result(Stanza2, 4, 1, true)
      end).

subscribe(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        escalus:send(Alice,
            escalus_stanza:presence_direct(hs_node(?ALICE), <<"available">>,
                                           [query_el(undefined)])),

        escalus:send(Alice,
                     add_to_u(pub_stanza(<<"new_item">>), Alice)),
        escalus:assert_many([is_iq_result, is_message],
                            escalus:wait_for_stanzas(Alice, 2)),

        timer:sleep(500),
        ensure_all_clean([Alice, Bob])
      end).

subscribe_version(Config) ->
    escalus:story(Config, [{alice, 1}, {carol, 1}],
      fun(Alice, Carol) ->

        V2 = hd(tl(proplists:get_value(alice_versions, Config))),

        escalus:send(Alice,
            escalus_stanza:presence_direct(hs_node(?ALICE), <<"available">>,
                                           [query_el(V2)])),

        lists:foreach(
          fun(_) ->
                  escalus:assert(is_message, escalus:wait_for_stanza(Alice))
          end, lists:seq(1, 4)),

        %% Carol should get nothing from her own HS (since it's empty) nor from
        %% Alice's (since it's not his)
        escalus:send(Carol,
            escalus_stanza:presence_direct(hs_node(?CAROL), <<"available">>,
                                           [query_el(V2)])),
        escalus:send(Carol,
            escalus_stanza:presence_direct(hs_node(?ALICE), <<"available">>,
                                           [query_el(V2)])),

        escalus:send(Alice,
                     add_to_u(pub_stanza(<<"new_item2">>), Alice)),
        escalus:assert_many([is_iq_result, is_message],
                            escalus:wait_for_stanzas(Alice, 2)),
        timer:sleep(500),

        ensure_all_clean([Alice, Carol])
      end).

unsubscribe(Config) ->
    escalus:story(Config, [{alice, 1}],
      fun(Alice) ->
        % Alice's previous temp subscription should have been cleared by
        % her reconnection
        expect_iq_success_u(pub_stanza(<<"unpushed_item">>), Alice, Alice),
        timer:sleep(500),
        ensure_all_clean([Alice]),

        escalus:send(Alice,
            escalus_stanza:presence_direct(hs_node(?ALICE), <<"available">>,
                                           [query_el(undefined)])),

        escalus:send(Alice,
                     add_to_u(pub_stanza(<<"pushed_item">>), Alice)),
        escalus:assert_many([is_iq_result, is_message],
                            escalus:wait_for_stanzas(Alice, 2)),

        escalus:send(Alice,
            escalus_stanza:presence_direct(hs_node(?ALICE), <<"unavailable">>,
                                           [query_el(undefined)])),

        expect_iq_success_u(pub_stanza(<<"new_item3">>), Alice, Alice),
        timer:sleep(500),

        ensure_all_clean([Alice])
      end).

maintain_subscription(Config) ->
    AliceSpec = [{manual_ack, true}, {resource, <<"res1">>}
                 | escalus_users:get_userspec(Config, alice)],
    escalus:fresh_story(Config, [], fun() ->
        Steps = [start_stream,
                 stream_features,
                 maybe_use_ssl,
                 authenticate,
                 bind,
                 session,
                 stream_resumption],
        {ok, Alice, _} = escalus_connection:start(AliceSpec, Steps),

        escalus:send(Alice,
                     escalus_stanza:presence_direct(
                       hs_node(?ALICE), <<"available">>,
                       [query_el(undefined)])),

        % Give the subscription time to take before we axe the connection
        timer:sleep(250),

        escalus_client:kill_connection(Config, Alice),

        SMID = proplists:get_value(smid, Alice#client.props),

        Steps2 = [start_stream,
                  stream_features,
                  maybe_use_ssl,
                  authenticate,
                  mk_resume_stream(SMID, 0)],
        {ok, Alice2, _} = escalus_connection:start(AliceSpec, Steps2),

        escalus:send(Alice2,
                     add_to_u(pub_stanza(<<"pushed_item">>), Alice2)),

        Stanzas1 = escalus:wait_for_stanzas(Alice2, 2),
        escalus:send(Alice2, escalus_stanza:sm_ack(1)),
        Stanzas2 = escalus:wait_for_stanzas(Alice2, 2),
        escalus:send(Alice2, escalus_stanza:sm_ack(2)),

        escalus:assert_many([is_iq_result, is_sm_ack_request,
                             is_message, is_sm_ack_request],
                            Stanzas1 ++ Stanzas2),

        ct:log("Stanzas: ~p", [Stanzas1 ++ Stanzas2]),
        timer:sleep(250),

        escalus_client:kill_connection(Config, Alice2)
    end).

get_item(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, _Bob) ->
        Stanza = expect_iq_success_u(get_hs_stanza(?ITEM), Alice, Alice),
        test_helper:check_single_hs_result(Stanza, ?ITEM),

        % Deleted and never-existed items should both return not-found
        expect_iq_error_u(get_hs_stanza(<<"some_id">>), Alice, Alice),
        expect_iq_error_u(get_hs_stanza(?wocky_id:new()), Alice, Alice)
      end).

auto_publish_newly_public_bot(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1},
                           {karen, 1}, {tim, 1}],
      fun(Alice, Bob, Carol, Karen, Tim) ->
        %% This is not a good way to drop the Alice/Tim friendship, however
        %% I think our de-friend system is a bit broken at the moment -
        %% test_helper:remove_contact/2 results in an asymmetric pair of rosters
        %% whcih should never happpen. This workaround will suffice until
        %% that gets fixed.
        ?wocky_roster_item:delete(?ALICE, ?TIM),
        ?wocky_roster_item:delete(?TIM, ?ALICE),
        ?wocky_roster_item:delete(?ALICE, ?KAREN),
        ?wocky_roster_item:delete(?KAREN, ?ALICE),
        ?wocky_roster_item:delete(?KAREN, ?TIM),
        ?wocky_roster_item:delete(?TIM, ?KAREN),

        test_helper:subscribe(Tim, Alice),

        check_home_stream_sizes(?BOB_HS_ITEM_COUNT, [Bob], false),
        check_home_stream_sizes(3, [Alice], false),
        check_home_stream_sizes(0, [Carol, Tim]),

        Stanza = escalus_stanza:to(share_bot_stanza(), ?BJID(?BOB)),
        escalus_client:send(Alice, Stanza),
        timer:sleep(400),
        check_home_stream_sizes(?BOB_HS_ITEM_COUNT + 1, [Bob]),

        escalus_client:send(Alice, Stanza),
        timer:sleep(400),
        check_home_stream_sizes(?BOB_HS_ITEM_COUNT + 1, [Bob]),

        % Bob and Carol are friends and Tim is a subscriber so they all get
        % notified of the bot's new publicness - additionally Alice, as the
        % owner, also gets notified:
        set_bot_vis(?WOCKY_BOT_VIS_OWNER, Alice),
        set_bot_vis(?WOCKY_BOT_VIS_OPEN, Alice),
        check_home_stream_sizes(?BOB_HS_ITEM_COUNT + 1, [Bob]),
        check_home_stream_sizes(4, [Alice]),
        check_home_stream_sizes(1, [Carol, Tim]),

        % Karen is neither a friend nor subscriber, so gets nothing.
        check_home_stream_sizes(0, [Karen]),

        ensure_all_clean([Alice, Bob, Carol, Karen, Tim])
      end).

auto_publish_new_bot_friends(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}],
      fun(Alice, Bob, Carol) ->
        clear_home_streams(),
        expect_iq_success(bot_SUITE:create_stanza(safe_fields()), Alice),
        timer:sleep(400),
        check_home_stream_sizes(0, [Alice, Bob, Carol]),

        Fields = [{"public", "bool", true} | safe_fields()],
        expect_iq_success(bot_SUITE:create_stanza(Fields), Alice),
        timer:sleep(400),
        check_home_stream_sizes(1, [Alice, Bob, Carol], false)
      end).

auto_publish_new_bot_non_friends(Config) ->
    escalus:story(Config -- [{everyone_is_friends, true}],
                  [{alice, 1}, {karen, 1}, {tim, 1}],
      fun(Alice, Karen, Tim) ->
        clear_home_streams(),
        expect_iq_success(bot_SUITE:create_stanza(safe_fields()), Alice),
        timer:sleep(400),
        check_home_stream_sizes(0, [Alice, Karen, Tim]),

        Fields = [{"public", "bool", true} | safe_fields()],
        expect_iq_success(bot_SUITE:create_stanza(Fields), Alice),
        timer:sleep(400),
        check_home_stream_sizes(1, [Alice, Tim], false),
        check_home_stream_sizes(0, [Karen])
      end).


auto_publish_shared_private_bot(Config) ->
    escalus:story(Config -- [{everyone_is_friends, true}],
                  [{alice, 1}, {karen, 1}],
      fun(Alice, Karen) ->
        clear_home_streams(),
        check_home_stream_sizes(0, [Karen]),
        set_bot_vis(?WOCKY_BOT_VIS_OWNER, Alice),
        Stanza = escalus_stanza:to(share_bot_stanza(), ?BJID(?KAREN)),
        escalus_client:send(Alice, Stanza),
        timer:sleep(400),
        check_home_stream_sizes(1, [Karen])
      end).

auto_publish_bot_item(Config) ->
    escalus:story(Config, [{alice, 1}, {carol, 1}],
      fun(Alice, Carol) ->
        clear_home_streams(),
        set_bot_vis(?WOCKY_BOT_VIS_OPEN, Alice),
        check_home_stream_sizes(1, [Carol]),

        expect_iq_success(test_helper:subscribe_stanza(), Carol),

        expect_iq_success(
          publish_item_stanza(?BOT, <<"ID">>, <<"Title">>, <<"Content">>),
          Alice),

        check_home_stream_sizes(2, [Carol], false)

      end).

auto_publish_to_system_user(Config) ->
    ?wocky_user:add_role(?CAROL, ?wocky_user:system_role()),
    escalus:story(Config, [{alice, 1}, {carol, 1}],
      fun(Alice, Carol) ->
        clear_home_streams(),
        set_bot_vis(?WOCKY_BOT_VIS_OWNER, Alice),
        set_bot_vis(?WOCKY_BOT_VIS_OPEN, Alice),
        check_home_stream_sizes(1, [Carol]),

        expect_iq_success(test_helper:subscribe_stanza(), Carol),

        expect_iq_success(
          publish_item_stanza(?BOT, <<"ID">>, <<"Title">>, <<"Content">>),
          Alice),

        check_home_stream_sizes(2, [Carol], false)

      end),
    ?wocky_user:remove_role(?CAROL, ?wocky_user:system_role()).

bot_description_update(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}],
      fun(Alice, Bob, Carol) ->
        expect_iq_success(subscribe_stanza(), Carol),

        % New description should generate an HS item for non-owner subscribers
        clear_home_streams(),
        expect_iq_success(update_bot_desc_stanza("Updated Description"), Alice),
        timer:sleep(400),
        check_home_stream_sizes(0, [Alice, Bob]),
        expect_home_stream_bot_desc(Carol, false),

        % A description changing only whitespace should generate no HS items
        clear_home_streams(),
        expect_iq_success(
          update_bot_desc_stanza("\n Updat\red \tDes cription   "), Alice),
        timer:sleep(400),
        check_home_stream_sizes(0, [Alice, Carol, Bob]),

        % Blanking the description should not generate a notification
        clear_home_streams(),
        expect_iq_success(update_bot_desc_stanza("  \t  "), Alice),
        timer:sleep(400),
        check_home_stream_sizes(0, [Alice, Bob, Carol]),

        % Changing from a white-space only description should add the <new> tag
        clear_home_streams(),
        expect_iq_success(update_bot_desc_stanza("Fnord"), Alice),
        timer:sleep(400),
        check_home_stream_sizes(0, [Alice, Bob]),
        expect_home_stream_bot_desc(Carol, true),

        set_bot_vis(?WOCKY_BOT_VIS_OWNER, Alice)
      end).

bot_becomes_private(Config) ->
    escalus:story(Config, [{alice, 1}, {carol, 1}],
      fun(Alice, Carol) ->
        set_public(true, Alice),
        clear_home_streams(),

        expect_iq_success(subscribe_stanza(), Carol),

        % Place an item in Carol's HS about the bot
        expect_iq_success(update_bot_desc_stanza("Updated Description"), Alice),
        timer:sleep(400),
        expect_home_stream_bot_desc(Carol, false),

        % Make the bot private
        set_public(false, Alice),

        % The item should have been marked deleted on Carol's HS
        Stanza2 = expect_iq_success_u(get_hs_stanza(), Carol, Carol),
        check_hs_result(Stanza2, 0, 1, true)
      end).


no_auto_publish_pep_item(Config) ->
    mod_wocky_pep:register_handler(?NS_TEST, whitelist, ?MODULE),
    escalus:story(Config, [{alice, 1}],
      fun(Alice) ->
        clear_home_streams(),
        check_home_stream_sizes(0, [Alice]),

        Stanza = escalus_pubsub_stanza:publish(Alice, <<"test_item_id">>,
                                               pub_item(), <<"123">>,
                                               {pep, pub_node()}),
        escalus:send(Alice, Stanza),

        Received = escalus:wait_for_stanzas(Alice, 2),
        escalus:assert_many([is_message, is_iq_result], Received),

        check_home_stream_sizes(0, [Alice])

      end),
    mod_wocky_pep:unregister_handler(?NS_TEST, whitelist, ?MODULE).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

pub_stanza(ID) ->
    test_helper:iq_set(?NS_PUBLISHING,
                       #xmlel{name = <<"publish">>,
                              attrs = [{<<"node">>, ?HOME_STREAM_NODE}],
                              children = [new_item(ID)]}).

delete_stanza() ->
    test_helper:iq_set(?NS_PUBLISHING,
                       #xmlel{name = <<"publish">>,
                              attrs = [{<<"node">>, ?HOME_STREAM_NODE}],
                              children = [delete_item()]}).

share_bot_stanza() ->
    #xmlel{name = <<"message">>,
           attrs = [{<<"type">>, <<"headline">>}],
           children = [#xmlel{name = <<"bot">>,
                              attrs = [{<<"xmlns">>, ?NS_BOT}],
                              children = share_children()}]}.

share_children() ->
    [cdata_el(<<"jid">>, ?BOT_B_JID),
     cdata_el(<<"id">>, ?BOT),
     cdata_el(<<"server">>, ?SERVER),
     cdata_el(<<"action">>, <<"share">>)].

new_item(ID) ->
    #xmlel{name = <<"item">>,
           attrs = maybe_id(ID),
           children = [cdata_el(<<"new-published-element">>,
                                <<"hello there!">>),
                       #xmlel{name = <<"second-element">>,
                              attrs = [{<<"test_attr">>, <<"abc">>}]}]}.

cdata_el(Name, CData) ->
    #xmlel{name = Name, children = [#xmlcdata{content = CData}]}.

maybe_id(undefined) -> [];
maybe_id(ID) -> [{<<"id">>, ID}].

delete_item() ->
    #xmlel{name = <<"delete">>,
           attrs = [{<<"id">>, <<"some_id">>}]}.

is_presence_error(Stanza) ->
    escalus_pred:is_presence(Stanza)
    andalso
    escalus_pred:is_error(<<"cancel">>, <<"not-acceptable">>, Stanza).

set_bot_vis(Vis, Client) ->
    expect_iq_success(bot_SUITE:change_visibility_stanza(?BOT, Vis), Client).

expect_home_stream_bot_desc(Client, New) ->
    S = expect_iq_success_u(get_hs_stanza(), Client, Client),
    ct:log("S: ~p", [S]),
    El = #xmlel{} = xml:get_path_s(S, [{elem, <<"items">>},
                                       {elem, <<"item">>},
                                       {elem, <<"message">>},
                                       {elem, <<"bot-description-changed">>}]),
    case {New, xml:get_path_s(El, [{elem, <<"new">>}])} of
        {true, #xmlel{}} -> ok;
        {false, <<>>} -> ok;
        E -> ct:fail("Incorrect <new> tag: ~p", [E])
    end.

clear_home_streams() ->
    ?wocky_repo:delete_all(?wocky_home_stream_item).

pub_item() ->
    #xmlel{name = <<"pep-test-item">>,
           attrs = [{<<"xmlns">>, ?NS_TEST}]}.

pub_node() -> ?NS_TEST.

mk_resume_stream(SMID, PrevH) ->
    fun (Conn, Features) ->
            escalus_connection:send(Conn, escalus_stanza:resume(SMID, PrevH)),
            Resumed = escalus_connection:get_stanza(Conn, get_resumed),
            true = escalus_pred:is_sm_resumed(SMID, Resumed),
            Props = Conn#client.props,
            {Conn#client{props = [{smid, SMID} | Props]}, Features}
    end.

%%--------------------------------------------------------------------
%% Identity PEP hook
%%--------------------------------------------------------------------

handle_pep(_From, Element) ->
    Element.

%%--------------------------------------------------------------------
%% DB seeding
%%--------------------------------------------------------------------

seed_home_stream(Config) ->
    #{updated_at := V1, key := K1} =
        ok(?wocky_home_stream_item:put(?ALICE, ?BOT,
                                       ?BOT_B_JID, ?BOT_UPDATE_STANZA)),
    #{updated_at := V2, key := K2} =
        ok(?wocky_home_stream_item:put(?ALICE, ?ITEM,
                                       ?BOT_B_JID, ?ITEM_STANZA)),
    #{updated_at := V3, key := K3} =
        ok(?wocky_home_stream_item:put(?ALICE, ?ITEM2,
                                       ?BOT_B_JID, ?ITEM_STANZA2)),
    Versions = [?wocky_timestamp:to_string(V) || V <- [V1, V2, V3]],

    lists:foreach(
      fun (_) ->
              ?wocky_home_stream_item:put(?BOB, ?wocky_id:new(),
                                          ?BJID(?BOB), ?BOT_UPDATE_STANZA)
      end,
      lists:seq(1, ?BOB_HS_ITEM_COUNT)),
    [{alice_versions, Versions}, {alice_keys, [K1, K2, K3]} | Config].

ok({ok, Val}) -> Val.

update_bot_desc_stanza(Desc) ->
    test_helper:iq_set(
      ?NS_BOT, node_el(?BOT, <<"fields">>, [modify_field(Desc)])).

modify_field(Desc) ->
    bot_SUITE:create_field({"description", "string", Desc}).

safe_fields() ->
    lists:keyreplace("shortname", 1, bot_SUITE:default_fields(),
                     {"shortname", "string", ""}).

set_public(Public, Client) ->
    Stanza = test_helper:iq_set(
               ?NS_BOT, node_el(?BOT, <<"fields">>,
                                [bot_SUITE:create_field(
                                   {"public", "bool", Public})])),
    expect_iq_success(Stanza, Client).
