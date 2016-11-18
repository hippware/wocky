%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite for geolocation related functionality
-module(location_SUITE).
-compile(export_all).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("common_test/include/ct.hrl").

-include("wocky.hrl").
-include("wocky_db_seed.hrl").


%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() -> [geoloc,
          bad_geoloc,
          end_geoloc,
          xmpp_notification].

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
    wocky_db:clear_user_tables(?LOCAL_CONTEXT),
    Users = escalus:get_users([alice, bob, carol, karen]),
    Config1 = escalus:create_users(Config, Users),
    escalus:init_per_testcase(CaseName, Config1).

end_per_testcase(CaseName, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob, carol, karen])),
    escalus:end_per_testcase(CaseName, Config).


%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

geoloc(Config) ->
    %% Geoloc is whitelist, which currently means owner-only.
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}],
                  fun (Alice, Bob, Carol) ->
        test_helper:subscribe(Alice, Bob),
        Stanza = escalus_pubsub_stanza:publish(Alice, <<"abcedfg">>,
                                               geoloc_item(), <<"123">>,
                                               {pep, ?NS_GEOLOC}),
        escalus:send(Alice, Stanza),
        Received = escalus:wait_for_stanzas(Alice, 2),
        escalus:assert_many([is_message, is_iq_result], Received),
        test_helper:ensure_all_clean([Bob, Carol])
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
        test_helper:ensure_all_clean([Bob, Carol])
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
        test_helper:ensure_all_clean([Bob, Carol])
    end).

xmpp_notification(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        insert_bot(Alice),
        enable_push_notifications(Alice),

        %% Alice sends an XMPP location update...
        Stanza = escalus_pubsub_stanza:publish(Alice, <<"abcedfg">>,
                                               geoloc_item(), <<"123">>,
                                               {pep, ?NS_GEOLOC}),
        escalus:send(Alice, Stanza),
        Received = escalus:wait_for_stanzas(Alice, 2),
        escalus:assert_many([is_message, is_iq_result], Received),

        %% ...and receives an XMPP notification message...
        Notification = escalus:wait_for_stanza(Alice),
        escalus:assert(is_message, Notification),

        %% ...with a type of "headline"...
        <<"headline">> =
            proplists:get_value(<<"type">>, Notification#xmlel.attrs),

        %% ...and a child element called "bot".
        #xmlel{children = [#xmlel{name = <<"bot">>}]} = Notification
    end).


%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

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

insert_bot(Client) ->
    Jid = escalus_client:short_jid(Client),
    'Elixir.Wocky.Factory':insert(bot, [{owner, Jid},
                                        {lat, 6.789},
                                        {lon, -77}]).

enable_push_notifications(Client) ->
    Jid = jid:make(escalus_client:username(Client),
                   escalus_client:server(Client),
                   escalus_client:resource(Client)),
    wocky_notification_handler:enable(Jid, <<"apple">>, <<"123456789">>).
