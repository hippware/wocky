%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite for home stream
-module(home_stream_SUITE).
-compile(export_all).

-include("wocky.hrl").
-include("wocky_bot.hrl").
-include("test_helper.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("wocky_db_seed.hrl").

-compile({parse_transform, fun_chain}).
-compile({parse_transform, cut}).
-compile({parse_transform, do}).

-import(test_helper, [expect_iq_success_u/3, expect_iq_error_u/3,
                      expect_iq_success/2, add_to_u/2,
                      ensure_all_clean/1, publish_item_stanza/4,
                      get_hs_stanza/0, get_hs_stanza/1,
                      check_hs_result/2, check_hs_result/4,
                      hs_query_el/1, hs_node/1]).

-define(NS_TEST, <<"test-item-ns">>).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() -> [
          get,
          get_first,
          publish,
          delete,
          no_auto_publish_chat,
          subscribe,
          subscribe_version,
          unsubscribe,
          get_item,
          auto_publish_bot,
          auto_publish_bot_item,
          no_auto_publish_pep_item
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
    wocky_db:clear_user_tables(?LOCAL_CONTEXT),
    wocky_db:clear_tables(?LOCAL_CONTEXT, [home_stream]),
    wocky_db:clear_tables(shared, [bot]),
    wocky_db_seed:seed_tables(?LOCAL_CONTEXT, [home_stream]),
    wocky_db_seed:seed_tables(shared, [bot]),
    Users = escalus:get_users(users()),
    fun_chain:first(Config,
        escalus:init_per_suite(),
        escalus:create_users(Users),
        test_helper:make_everyone_friends(Users)
    ).

end_per_suite(Config) ->
    escalus:delete_users(Config, escalus:get_users(users())),
    escalus:end_per_suite(Config).

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
        ?assertEqual(?HS_V_3, Item#item.version),
        test_helper:check_attr(<<"version">>, ?HS_V_3, Stanza)
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
        ?assertEqual({delete, <<"some_id">>}, lists:last(Items))
      end).

no_auto_publish_chat(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        Stanza = escalus_stanza:chat_to(?ALICE_B_JID, <<"All your base">>),
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
                                           [hs_query_el(undefined)])),

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
        escalus:send(Alice,
            escalus_stanza:presence_direct(hs_node(?ALICE), <<"available">>,
                                           [hs_query_el(?HS_V_2)])),

        lists:foreach(
          fun(_) ->
                  escalus:assert(is_message, escalus:wait_for_stanza(Alice))
          end, lists:seq(1, 4)),

        %% Carol should get nothing from her own HS (since it's empty) nor from
        %% Alice's (since it's not his)
        escalus:send(Carol,
            escalus_stanza:presence_direct(hs_node(?CAROL), <<"available">>,
                                           [hs_query_el(?HS_V_2)])),
        escalus:send(Carol,
            escalus_stanza:presence_direct(hs_node(?ALICE), <<"available">>,
                                           [hs_query_el(?HS_V_2)])),

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
        escalus:send(Alice,
            escalus_stanza:presence_direct(hs_node(?ALICE), <<"available">>,
                                           [hs_query_el(undefined)])),

        escalus:send(Alice,
            escalus_stanza:presence_direct(hs_node(?ALICE), <<"unavailable">>,
                                           [hs_query_el(undefined)])),

        expect_iq_success_u(pub_stanza(<<"new_item3">>), Alice, Alice),
        timer:sleep(500),

        ensure_all_clean([Alice])
      end).

get_item(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, _Bob) ->
        Stanza = expect_iq_success_u(get_hs_stanza(?ITEM), Alice, Alice),
        test_helper:check_single_hs_result(Stanza, ?ITEM),

        % Deleted and never-existed items should both return not-found
        expect_iq_error_u(get_hs_stanza(<<"some_id">>), Alice, Alice),
        expect_iq_error_u(get_hs_stanza(wocky_db:create_id()), Alice, Alice)
      end).

auto_publish_bot(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}, {tim, 1}],
      fun(Alice, Bob, Carol, Tim) ->
        %% This is not a good way to drop the Alice/Tim friendship, however
        %% I think our de-friend system is a bit broken at the moment -
        %% test_helper:remove_contact/2 results in an asymmetric pair of rosters
        %% whcih should never happpen. This workaround will suffice until
        %% that gets fixed.
        wocky_db_roster:delete_roster_item(?ALICE, ?LOCAL_CONTEXT, ?TIM_B_JID),
        wocky_db_roster:delete_roster_item(?TIM, ?LOCAL_CONTEXT, ?ALICE_B_JID),

        test_helper:subscribe(Tim, Alice),

        check_home_stream_sizes(?BOB_HS_ITEM_COUNT, [Bob], false),

        Stanza = escalus_stanza:to(share_bot_stanza(), ?BOB_B_JID),
        escalus_client:send(Alice, Stanza),
        timer:sleep(400),
        check_home_stream_sizes(?BOB_HS_ITEM_COUNT + 1, [Bob]),

        escalus_client:send(Alice, Stanza),
        timer:sleep(400),
        check_home_stream_sizes(?BOB_HS_ITEM_COUNT + 1, [Bob]),

        set_bot_vis(?WOCKY_BOT_VIS_OWNER, Alice),
        set_bot_vis(?WOCKY_BOT_VIS_WHITELIST, Alice),
        check_home_stream_sizes(?BOB_HS_ITEM_COUNT + 1, [Bob]),
        check_home_stream_sizes(0, [Carol, Tim]),

        clear_home_streams(),

        set_bot_vis(?WOCKY_BOT_VIS_OWNER, Alice),
        set_bot_vis(?WOCKY_BOT_VIS_FRIENDS, Alice),
        check_home_stream_sizes(1, [Bob, Carol]),
        check_home_stream_sizes(0, [Tim]),
        clear_home_streams(),

        set_bot_vis(?WOCKY_BOT_VIS_OWNER, Alice),
        set_bot_vis(?WOCKY_BOT_VIS_FOLLOWERS, Alice),
        check_home_stream_sizes(1, [Bob, Carol, Tim]),
        clear_home_streams(),

        set_bot_vis(?WOCKY_BOT_VIS_OWNER, Alice),
        set_bot_vis(?WOCKY_BOT_VIS_OPEN, Alice),
        check_home_stream_sizes(1, [Bob, Carol, Tim]),

        ensure_all_clean([Alice, Bob, Carol, Tim])
      end).

auto_publish_bot_item(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        check_home_stream_sizes(1, [Bob]),

        expect_iq_success(test_helper:subscribe_stanza(), Bob),

        expect_iq_success(
          publish_item_stanza(?BOT, <<"ID">>, <<"Title">>, <<"Content">>),
          Alice),

        check_home_stream_sizes(2, [Bob], false)

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
     cdata_el(<<"server">>, ?LOCAL_CONTEXT),
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

check_home_stream_sizes(ExpectedSize, Clients) ->
    check_home_stream_sizes(ExpectedSize, Clients, true).
check_home_stream_sizes(ExpectedSize, Clients, CheckLastContent) ->
    lists:foreach(
      fun(Client) ->
              S = expect_iq_success_u(get_hs_stanza(), Client, Client),
              I = check_hs_result(S, ExpectedSize, 0, ExpectedSize =/= 0),
              ExpectedSize =:= 0 orelse not CheckLastContent orelse
              escalus:assert(test_helper:is_bot_action(?BOT, _),
                             hd((lists:last(I))#item.stanzas))
      end, Clients).

clear_home_streams() ->
    wocky_db:truncate(?LOCAL_CONTEXT, home_stream).

pub_item() ->
    #xmlel{name = <<"pep-test-item">>,
           attrs = [{<<"xmlns">>, ?NS_TEST}]}.

pub_node() -> ?NS_TEST.

%%--------------------------------------------------------------------
%% Identity PEP hook
%%--------------------------------------------------------------------

handle_pep(_From, Element) ->
    Element.
