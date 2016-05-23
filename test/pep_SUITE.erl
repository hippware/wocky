%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite for mod_wocky_pep
-module(pep_SUITE).
-compile(export_all).

-include("wocky.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("wocky_db_seed.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [publish,
     geoloc,
     bad_geoloc,
     end_geoloc,
     bad_requests
    ].

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
    wocky_db_seed:clear_user_tables(?LOCAL_CONTEXT),
    Users = escalus:get_users([alice, bob, carol]),
    Config1 = escalus:create_users(Config, Users),
    escalus:init_per_testcase(CaseName, Config1).

end_per_testcase(CaseName, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob, carol])),
    escalus:end_per_testcase(CaseName, Config).


%%--------------------------------------------------------------------
%% mod_wocky_pep tests
%%--------------------------------------------------------------------

publish(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}],
                  fun (Alice, Bob, Carol) ->
        test_helper:subscribe(Alice, Bob),
        Stanza = escalus_pubsub_stanza:publish(Alice, <<"test_item_id">>,
                                               pub_item(), <<"123">>,
                                               {pep, pub_node()}),
        escalus:send(Alice, Stanza),
        Received = escalus:wait_for_stanzas(Alice, 2),
        escalus:assert_many([is_message, is_iq_result], Received),
        Recieved2 = escalus:wait_for_stanza(Bob),
        escalus:assert(is_message, Recieved2),
        assert_no_stanzas(Carol)
    end).

geoloc(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}],
                  fun (Alice, Bob, Carol) ->
        test_helper:subscribe(Alice, Bob),
        Stanza = escalus_pubsub_stanza:publish(Alice, <<"abcedfg">>,
                                               geoloc_item(), <<"123">>,
                                               {pep, ?NS_GEOLOC}),
        escalus:send(Alice, Stanza),
        Received = escalus:wait_for_stanzas(Alice, 2),
        escalus:assert_many([is_message, is_iq_result], Received),
        Recieved2 = escalus:wait_for_stanza(Bob),
        escalus:assert(is_message, Recieved2),
        assert_no_stanzas(Carol)
    end).

bad_geoloc(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}],
                  fun (Alice, Bob, Carol) ->
        test_helper:subscribe(Alice, Bob),
        Stanza = escalus_pubsub_stanza:publish(Alice, <<"abcedfg">>,
                                               bad_geoloc_item(), <<"123">>,
                                               {pep, ?NS_GEOLOC}),
        escalus:send(Alice, Stanza),
        Received = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, Received),
        assert_no_stanzas(Bob),
        assert_no_stanzas(Carol)
    end).

end_geoloc(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}],
                  fun (Alice, Bob, Carol) ->
        test_helper:subscribe(Alice, Bob),
        Stanza = escalus_pubsub_stanza:publish(Alice, <<"abcedfg">>,
                                               end_geoloc_item(), <<"123">>,
                                               {pep, ?NS_GEOLOC}),
        escalus:send(Alice, Stanza),
        Received = escalus:wait_for_stanzas(Alice, 2),
        escalus:assert_many([is_message, is_iq_result], Received),
        Recieved2 = escalus:wait_for_stanza(Bob),
        escalus:assert(is_message, Recieved2),
        assert_no_stanzas(Carol)
    end).

bad_requests(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        %% Non pubsub request
        Stanza =
        escalus_stanza:to(
          #xmlel{name = <<"iq">>,
                 attrs = [{<<"type">>, <<"set">>}],
                 children = [#xmlel{name = <<"blah">>,
                                    attrs = [{<<"xmlns">>,
                                              ?NS_PUBSUB}]}]},
          escalus_client:short_jid(Alice)),
        Result = escalus:send_and_wait(Alice, Stanza),
        escalus:assert(is_iq_error, Result),

        %% Non publish request
        Stanza2 =
        escalus_stanza:to(
          #xmlel{name = <<"iq">>,
                 attrs = [{<<"type">>, <<"set">>}],
                 children = [#xmlel{name = <<"pubsub">>,
                                    attrs = [{<<"xmlns">>,
                                              ?NS_PUBSUB}],
                                    children = [#xmlel{name = <<"blah">>}]}]},
          escalus_client:short_jid(Alice)),
        Result2 = escalus:send_and_wait(Alice, Stanza2),
        escalus:assert(is_iq_error, Result2),

        %% No node
        Stanza3 =
        escalus_stanza:to(
          #xmlel{name = <<"iq">>,
                 attrs = [{<<"type">>, <<"set">>}],
                 children = [#xmlel{name = <<"pubsub">>,
                                    attrs = [{<<"xmlns">>,
                                              ?NS_PUBSUB}],
                                    children = [#xmlel{name =
                                                       <<"publish">>}]}]},
          escalus_client:short_jid(Alice)),
        Result3 = escalus:send_and_wait(Alice, Stanza3),
        escalus:assert(is_iq_error, Result3)
    end).


%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

pub_item() ->
    #xmlel{name = <<"test-item">>,
           attrs = [{<<"xmlns">>, <<"test-item-ns">>}]}.

pub_node() -> <<"test_node">>.

assert_no_stanzas(User) ->
    ?assertNot(escalus_client:has_stanzas(User)).

geoloc_item() ->
    #xmlel{name = <<"geoloc">>,
           attrs = [{<<"xmlns">>, ?NS_GEOLOC}],
           children = geoloc_data()}.

bad_geoloc_item() ->
    #xmlel{name = <<"geoloc">>,
           attrs = [{<<"xmlns">>, ?NS_GEOLOC}],
           children = tl(geoloc_data())}.

end_geoloc_item() ->
    #xmlel{name = <<"geoloc">>,
           attrs = [{<<"xmlns">>, ?NS_GEOLOC}],
           children = []}.

geoloc_data() ->
    [cdata_item(<<"lat">>, <<"6.789">>),
     cdata_item(<<"lon">>, <<"-77">>),
     cdata_item(<<"accuracy">>, <<"1.23">>)].

cdata_item(Name, Val) ->
    #xmlel{name = Name,
           children = [#xmlcdata{content = Val}]}.
