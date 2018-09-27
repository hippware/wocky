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
                      retract_item_stanza/2,
                      get_hs_stanza/0, get_hs_stanza/1,
                      check_hs_result/2, check_hs_result/3, check_hs_result/4,
                      query_el/1, hs_node/1, node_el/3,
                      subscribe_stanza/0, check_home_stream_sizes/2,
                      check_home_stream_sizes/3, watch_hs/1, watch_hs/2,
                      unwatch_hs/1]).

-define(NS_TEST, <<"test-item-ns">>).
-define(BOB_HS_ITEM_COUNT, 25).
-define(BOT_UPDATE_STANZA, <<"<bot_update>A thing happened</bot_update>">>).


%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() -> [
          get,
          get_first,
          get_with_rsm,
          catchup,
          publish,
          delete,
          no_auto_publish_chat,
          watch,
          watch_with_version,
          unsubscribe,
          maintain_subscription,
          get_item,
          {group, publish_bot},
          no_auto_publish_pep_item,
          {group, catchup_limits}
         ].

groups() ->
    [{publish_bot, [], publish_bot_cases()},
     {catchup_limits, [], catchup_limits_cases()}].

publish_bot_cases() ->
    [
     auto_publish_bot_item,
     bot_description_update,
     bot_change_notification
    ].

catchup_limits_cases() ->
    [
     catchup_command_limits,
     subscribe_command_limits
    ].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

users() ->
    [alice, bob, carol, tim].

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),
    test_helper:disable_push_reflection(),
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
init_per_group(catchup_limits, Config) ->
    clear_home_streams(),
    User = ?wocky_repo:get(?wocky_user, ?ALICE),
    lists:foreach(
      fun(_) ->
              ?wocky_factory:insert(home_stream_item, #{user => User})
      end,
      lists:seq(1, 200)),
    Config;
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
        check_hs_result(Stanza2, 0, false)
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

catchup(Config) ->
    escalus:story(Config, [{alice, 1}, {carol, 1}],
      fun(Alice, Carol) ->
        V2 = hd(tl(proplists:get_value(alice_versions, Config))),
        Stanza = expect_iq_success_u(catchup_stanza(V2), Alice, Alice),
        check_hs_result(Stanza, 1, true, 0),

        expect_iq_error_u(catchup_stanza(V2), Carol, Alice),

        expect_iq_error_u(catchup_stanza(<<"2016-12-31T23:00:00Z">>),
                          Alice, Alice),

        % Invalid versions should cause an error
        expect_iq_error_u(catchup_stanza(?wocky_id:new()), Alice, Alice)
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

        expect_iq_success_u(delete_stanza(<<"some_id">>), Alice, Alice),

        % Bob can't delete from Alice's home stream
        expect_iq_error_u(delete_stanza(<<"some_id">>), Bob, Alice),

        Stanza2 = expect_iq_success_u(get_hs_stanza(), Alice, Alice),
        check_hs_result(Stanza2, 4)
      end).

no_auto_publish_chat(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        Stanza = escalus_stanza:chat_to(?BJID(?ALICE), <<"All your base">>),
        escalus:send(Bob, Stanza),
        escalus:assert(is_chat_message, escalus_client:wait_for_stanza(Alice)),

        Stanza2 = expect_iq_success_u(get_hs_stanza(), Alice, Alice),
        check_hs_result(Stanza2, 4)
      end).

watch(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        watch_hs(Alice),

        % Both published and deleted items should be sent to watchers
        escalus:send(Alice, add_to_u(pub_stanza(<<"new_item">>), Alice)),
        S = escalus:wait_for_stanzas(Alice, 2),
        escalus:assert_many([is_iq_result, is_message], S),
        % The first publication should have the `new='true'` attribute

        escalus:send(Alice, add_to_u(pub_stanza(<<"new_item">>), Alice)),
        S2 = escalus:wait_for_stanzas(Alice, 2),
        escalus:assert_many([is_iq_result, is_message], S2),
        % The second publication should not have the `new='true'` attribute

        escalus:send(Alice, add_to_u(delete_stanza(<<"new_item">>), Alice)),
        S3 = escalus:wait_for_stanzas(Alice, 2),
        escalus:assert_many([is_iq_result, is_message], S3),
        check_is_deletion(S3),

        unwatch_hs(Alice),
        timer:sleep(500),
        ensure_all_clean([Alice, Bob])
      end).

watch_with_version(Config) ->
    escalus:story(Config, [{alice, 1}, {carol, 1}],
      fun(Alice, Carol) ->

        V2 = hd(tl(proplists:get_value(alice_versions, Config))),

        watch_hs(Alice, V2),

        lists:foreach(
          fun(_) ->
                  escalus:assert(is_message, escalus:wait_for_stanza(Alice))
          end, lists:seq(1, 4)),

        %% Carol should get nothing from her own HS (since it's empty) and an
        %% auth error from Alice's (since it's not hers)
        watch_hs(Carol, V2),

        escalus:send(Carol,
            escalus_stanza:presence_direct(hs_node(?ALICE), <<"available">>,
                                           [query_el(V2)])),
        escalus:assert(is_presence_auth_error(_),
                       escalus:wait_for_stanza(Carol)),

        escalus:send(Alice,
                     add_to_u(pub_stanza(<<"new_item2">>), Alice)),
        escalus:assert_many([is_iq_result, is_message],
                            escalus:wait_for_stanzas(Alice, 2)),

        % Before the magic time we should get an error
        watch_hs(Alice, <<"2016-12-31T23:00:00Z">>),

        escalus:assert(is_presence_error(_), escalus:wait_for_stanza(Alice)),

        % An invalid version should cause an error
        watch_hs(Alice, ?wocky_id:new()),

        escalus:assert(is_presence_error(_), escalus:wait_for_stanza(Alice)),

        timer:sleep(500),
        unwatch_hs(Alice),
        unwatch_hs(Carol),
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

        watch_hs(Alice),

        escalus:send(Alice,
                     add_to_u(pub_stanza(<<"pushed_item">>), Alice)),
        escalus:assert_many([is_iq_result, is_message],
                            escalus:wait_for_stanzas(Alice, 2)),

        unwatch_hs(Alice),

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

        watch_hs(Alice),

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

        unwatch_hs(Alice2),
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

auto_publish_bot_item(Config) ->
    test_helper:add_invitation(?BOT, ?CAROL, ?ALICE),
    escalus:story(Config, [{alice, 1}, {carol, 1}],
      fun(Alice, Carol) ->
        clear_home_streams(),
        timer:sleep(400),
        check_home_stream_sizes(0, [Carol]),

        expect_iq_success(test_helper:subscribe_stanza(), Carol),

        ItemID = get_item_id(
          expect_iq_success(
            publish_item_stanza(?BOT, <<"ID">>, <<"Title">>, <<"Content">>),
            Alice)
         ),

        check_home_stream_sizes(1, [Carol], false),

        expect_iq_success(retract_item_stanza(?BOT, ItemID), Alice),
        timer:sleep(400),
        check_home_stream_sizes(0, [Carol], false)

      end).

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
        expect_home_stream_bot_desc(Carol, true)
      end).

bot_change_notification(Config) ->
    escalus:story(Config, [{alice, 1}, {carol, 1}],
      fun(Alice, Carol) ->
        clear_home_streams(),

        watch_hs(Carol),

        timer:sleep(400),

        % Update description
        expect_iq_success(update_bot_desc_stanza("Different description"),
                          Alice),
        escalus:assert(fun is_bot_desc_change_notification/1,
                       escalus:wait_for_stanza(Carol)),
        timer:sleep(400),
        ensure_all_clean([Alice, Carol]),

        % Update title
        expect_iq_success(
          update_field_stanza("title", "string", "newtitle"), Alice),
        escalus:assert(fun is_bot_ref_change_notification/1,
                       escalus:wait_for_stanza(Carol)),
        timer:sleep(400),
        ensure_all_clean([Alice, Carol]),

        % Publish item
        ItemID = bot_SUITE:publish_item(?BOT, <<"ignored">>, <<"title">>,
                               <<"content">>, undefined, Alice),
        escalus:assert_many([fun is_bot_ref_change_notification/1,
                             fun is_item_publish_notification/1],
                            escalus:wait_for_stanzas(Carol, 3)),
        timer:sleep(400),
        ensure_all_clean([Alice, Carol]),

        % Retract item
        bot_SUITE:retract_item(?BOT, ItemID, Alice),
        escalus:assert_many([fun is_hs_item_deleted_notification/1,
                             fun is_bot_ref_change_notification/1],
                            escalus:wait_for_stanzas(Carol, 2)),
        timer:sleep(400),
        ensure_all_clean([Alice, Carol]),

        % Update address
        expect_iq_success(
          update_field_stanza("address", "string", "hereabouts"), Alice),
        escalus:assert(fun is_bot_ref_change_notification/1,
                       escalus:wait_for_stanza(Carol)),
        timer:sleep(400),
        ensure_all_clean([Alice, Carol]),

        % No updated generated for address_data change
        expect_iq_success(
          update_field_stanza("address_data", "string", "hereabouts"), Alice),
        timer:sleep(400),

        unwatch_hs(Alice),
        ensure_all_clean([Alice, Carol])
      end).

is_bot_ref_change_notification(S) ->
    escalus_pred:is_message(S)
    andalso
    <<>> =/= xml:get_path_s(S, [{elem, <<"notification">>},
                                {elem, <<"reference-changed">>},
                                {elem, <<"bot">>}]).

is_bot_desc_change_notification(S) ->
    escalus_pred:is_message(S)
    andalso
    <<>> =/= xml:get_path_s(S, [{elem, <<"notification">>},
                                {elem, <<"item">>},
                                {elem, <<"message">>},
                                {elem, <<"bot-description-changed">>}]).

is_item_publish_notification(S) ->
    escalus_pred:is_message(S)
    andalso
    <<>> =/= xml:get_path_s(S, [{elem, <<"notification">>},
                                {elem, <<"item">>},
                                {elem, <<"message">>},
                                {elem, <<"event">>},
                                {elem, <<"item">>},
                                {elem, <<"entry">>}]).

is_hs_item_deleted_notification(S) ->
    escalus_pred:is_message(S)
    andalso
    <<>> =/= xml:get_path_s(S, [{elem, <<"notification">>},
                                {elem, <<"delete">>}]).

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

catchup_command_limits(Config) ->
    escalus:story(Config, [{alice, 1}],
      fun(Alice) ->
        fun_chain:first([{days, -1}],
                        ?wocky_timestamp:shift(),
                        ?wocky_timestamp:to_string(),
                        catchup_stanza(),
                        expect_iq_error_u(Alice, Alice)
                       )
      end).

subscribe_command_limits(Config) ->
    escalus:story(Config, [{alice, 1}],
      fun(Alice) ->
        fun_chain:last([{days, -1}],
                       ?wocky_timestamp:shift(),
                       ?wocky_timestamp:to_string(),
                       watch_hs(Alice)),

        escalus:assert(is_presence_error(_), escalus:wait_for_stanza(Alice))
      end).


%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

pub_stanza(ID) ->
    test_helper:iq_set(?NS_PUBLISHING,
                       #xmlel{name = <<"publish">>,
                              attrs = [{<<"node">>, ?HOME_STREAM_NODE}],
                              children = [new_item(ID)]}).

delete_stanza(ID) ->
    test_helper:iq_set(?NS_PUBLISHING,
                       #xmlel{name = <<"publish">>,
                              attrs = [{<<"node">>, ?HOME_STREAM_NODE}],
                              children = [delete_item(ID)]}).

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

delete_item(ID) ->
    #xmlel{name = <<"delete">>,
           attrs = [{<<"id">>, ID}]}.

is_presence_error(Stanza) ->
    escalus_pred:is_presence(Stanza)
    andalso
    (
     escalus_pred:is_error(<<"modify">>, <<"not-acceptable">>, Stanza)
     orelse
     escalus_pred:is_error(<<"modify">>, <<"bad-request">>, Stanza)
    ).

is_presence_auth_error(Stanza) ->
    escalus_pred:is_presence(Stanza)
    andalso
    escalus_pred:is_error(<<"auth">>, <<"forbidden">>, Stanza).

set_bot_vis(Vis, Client) ->
    expect_iq_success(bot_SUITE:change_visibility_stanza(?BOT, Vis), Client).

expect_home_stream_bot_desc(Client, New) ->
    S = expect_iq_success_u(get_hs_stanza(), Client, Client),
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
        ok(?wocky_home_stream:put(?ALICE, ?BOT,
                                       ?BOT_B_JID, ?BOT_UPDATE_STANZA)),
    #{updated_at := V2, key := K2} =
        ok(?wocky_home_stream:put(?ALICE, ?ITEM,
                                       ?BOT_B_JID, ?ITEM_STANZA)),
    #{updated_at := V3, key := K3} =
        ok(?wocky_home_stream:put(?ALICE, ?ITEM2,
                                       ?BOT_B_JID, ?ITEM_STANZA2)),
    Versions = [?wocky_timestamp:to_string(V) || V <- [V1, V2, V3]],

    lists:foreach(
      fun (_) ->
              ?wocky_home_stream:put(?BOB, ?wocky_id:new(),
                                          ?BJID(?BOB), ?BOT_UPDATE_STANZA)
      end,
      lists:seq(1, ?BOB_HS_ITEM_COUNT)),
    [{alice_versions, Versions}, {alice_keys, [K1, K2, K3]} | Config].

ok({ok, Val}) -> Val.

update_bot_desc_stanza(Desc) ->
    update_field_stanza("description", "string", Desc).

update_field_stanza(Name, Type, Value) ->
    test_helper:iq_set(
      ?NS_BOT, node_el(?BOT, <<"fields">>,
                       [bot_SUITE:old_create_field({Name, Type, Value})])).

safe_fields() ->
    lists:keyreplace("shortname", 1, bot_SUITE:default_fields(),
                     {"shortname", "string", ""}).

check_is_deletion(Stanzas) ->
    Stanza = get_message_stanza(Stanzas),
    #xmlel{} = xml:get_path_s(Stanza, [{elem, <<"notification">>},
                                       {elem, <<"delete">>}]).

get_message_stanza(Stanzas) ->
    [Stanza] = lists:filter(escalus_pred:is_message(_), Stanzas),
    Stanza.

catchup_stanza(Version) ->
    test_helper:iq_get(?NS_PUBLISHING,
                       #xmlel{name = <<"catchup">>,
                              attrs = [{<<"node">>, ?HOME_STREAM_NODE},
                                       {<<"version">>, Version}]}).

get_item_id(Stanza) ->
    xml:get_path_s(Stanza, [{elem, <<"item_id">>}, cdata]).
