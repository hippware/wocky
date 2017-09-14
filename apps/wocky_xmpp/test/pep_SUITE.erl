%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite for mod_wocky_pep
-module(pep_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("test_helper.hrl").

-define(NS_TEST, <<"test-item-ns">>).


%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [publish_presence,
     publish_roster,
     bad_requests].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
    Config1 = test_helper:setup_users(Config, [alice, bob, carol]),
    escalus:init_per_testcase(CaseName, Config1).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).


%%--------------------------------------------------------------------
%% mod_wocky_pep tests
%%--------------------------------------------------------------------

publish_presence(Config) ->
    mod_wocky_pep:register_handler(?NS_TEST, presence, ?MODULE),
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}],
                  fun (Alice, Bob, Carol) ->
        test_helper:subscribe_pair(Alice, Bob),
        Stanza = escalus_pubsub_stanza:publish(Alice, <<"test_item_id">>,
                                               pub_item(), <<"123">>,
                                               {pep, pub_node()}),
        escalus:send(Alice, Stanza),
        Received = escalus:wait_for_stanzas(Alice, 2),
        escalus:assert_many([is_message, is_iq_result], Received),
        Recieved2 = escalus:wait_for_stanza(Bob),
        escalus:assert(is_message, Recieved2),
        test_helper:ensure_all_clean([Carol])
    end),
    mod_wocky_pep:unregister_handler(?NS_TEST, presence, ?MODULE).

publish_roster(Config) ->
    mod_wocky_pep:register_handler(?NS_TEST, roster, ?MODULE),
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}],
                  fun (Alice, Bob, Carol) ->
        test_helper:subscribe(Bob, Alice),
        test_helper:subscribe(Carol, Alice),
        test_helper:add_contact(Alice, Bob, [], <<"Bobbie">>),
        test_helper:add_contact(Alice, Carol, [], <<"Car">>),
        Stanza = escalus_pubsub_stanza:publish(Alice, <<"test_item_id">>,
                                               pub_item(), <<"123">>,
                                               {pep, pub_node()}),
        escalus:send(Alice, Stanza),
        Received = escalus:wait_for_stanzas(Alice, 2),
        escalus:assert_many([is_message, is_iq_result], Received),
        Recieved2 = escalus:wait_for_stanza(Bob),
        escalus:assert(is_message, Recieved2),
        Recieved3 = escalus:wait_for_stanza(Carol),
        escalus:assert(is_message, Recieved3)

    end),
    mod_wocky_pep:unregister_handler(?NS_TEST, roster, ?MODULE).


bad_requests(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        %% Non pubsub request
        Stanza =
        escalus_stanza:to(
          test_helper:iq_set(?NS_PUBSUB,
                             #xmlel{name = <<"blah">>}),
          escalus_client:short_jid(Alice)),
        Result = escalus:send_and_wait(Alice, Stanza),
        escalus:assert(is_iq_error, Result),

        %% Non publish request
        Stanza2 =
        escalus_stanza:to(
          test_helper:iq_set(?NS_PUBSUB,
                             #xmlel{name = <<"pubsub">>,
                                    children = [#xmlel{name = <<"blah">>}]}),
          escalus_client:short_jid(Alice)),
        Result2 = escalus:send_and_wait(Alice, Stanza2),
        escalus:assert(is_iq_error, Result2),

        %% No node
        Stanza3 =
        escalus_stanza:to(
          test_helper:iq_set(?NS_PUBSUB,
                             #xmlel{name = <<"pubsub">>,
                                    children = [#xmlel{name =
                                                       <<"publish">>}]}),
          escalus_client:short_jid(Alice)),
        Result3 = escalus:send_and_wait(Alice, Stanza3),
        escalus:assert(is_iq_error, Result3)
    end).


%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

pub_item() ->
    #xmlel{name = <<"test-item">>,
           attrs = [{<<"xmlns">>, ?NS_TEST}]}.

pub_node() -> ?NS_TEST.

%%--------------------------------------------------------------------
%% Identity PEP hook
%%--------------------------------------------------------------------

handle_pep(_From, Element) -> Element.
