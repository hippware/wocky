%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite for geolocation related functionality
-module(location_SUITE).
-compile(export_all).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("common_test/include/ct.hrl").

-include("wocky.hrl").
-include("wocky_db_seed.hrl").
-include("test_helper.hrl").
-include("wocky_bot.hrl").

-import(test_helper, [expect_iq_success_u/3,
                      get_hs_stanza/0,
                      check_hs_result/2,
                      check_hs_result/4]).


%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() -> [geoloc,
          bad_geoloc,
          end_geoloc,
          xmpp_notification,
          bot_follower_notification
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
                                               geoloc_item(loc_test), <<"123">>,
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
                                               bad_geoloc_item(loc_test),
                                               <<"123">>,
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
        wocky_db:truncate(?LOCAL_CONTEXT, home_stream),
        insert_bot(Alice),

        %% Alice's home stream is empty to start
        Stanza = expect_iq_success_u(get_hs_stanza(), Alice, Alice),
        check_hs_result(Stanza, 0, 0, false),

        %% Alice sends an XMPP location update...
        Stanza2 = escalus_pubsub_stanza:publish(Alice, <<"abcedfg">>,
                                                geoloc_item(loc_test),
                                                <<"123">>,
                                                {pep, ?NS_GEOLOC}),
        escalus:send(Alice, Stanza2),
        Received = escalus:wait_for_stanzas(Alice, 2),
        escalus:assert_many([is_message, is_iq_result], Received),

        %% ...and one new item shows up in the home stream. This is the
        %% location update. There is no bot entry notification because
        %% Alice is both the owner of the bot and the user entering it.
        Stanza3 = expect_iq_success_u(get_hs_stanza(), Alice, Alice),
        Items = check_hs_result(Stanza3, 1),

        %% ...with the last item being a message.
        escalus:assert(is_message, hd((lists:last(Items))#item.stanzas))
    end).

bot_follower_notification(Config) ->
    wocky_db_seed:seed_tables(shared, [bot, bot_subscriber]),
    escalus:story(Config, [{alice, 1}, {carol, 1}], fun(Alice, Carol) ->
        test_helper:subscribe_pair(Alice, Carol),

        bot_SUITE:set_visibility(Alice, ?WOCKY_BOT_VIS_OPEN, ?BOT),

        wocky_db:truncate(?LOCAL_CONTEXT, home_stream),
        wocky_db:truncate(?LOCAL_CONTEXT, bot_event),

        %% Carol sends an XMPP location update...
        Stanza2 = escalus_pubsub_stanza:publish(Carol, <<"abcedfg">>,
                                                geoloc_item(bot_test),
                                                <<"123">>,
                                                {pep, ?NS_GEOLOC}),
        escalus:send(Carol, Stanza2),
        Received = escalus:wait_for_stanzas(Carol, 2),
        escalus:assert_many([is_message, is_iq_result], Received),
        timer:sleep(500),

        %% ...and one new items show up in each user's home stream:
        %% Alice gets the bot entry notification; Carol gets the location
        %% notification.
        lists:foreach(
          fun (Client) ->
              Stanza = expect_iq_success_u(get_hs_stanza(), Client, Client),
              Items = check_hs_result(Stanza, 1),
              escalus:assert(is_message, hd((lists:last(Items))#item.stanzas))
          end, [Alice, Carol])
    end).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

geoloc_item(Data) ->
    #xmlel{name = <<"geoloc">>,
           attrs = [{<<"xmlns">>, ?NS_GEOLOC}],
           children = geoloc_data(Data)}.

bad_geoloc_item(Data) ->
    #xmlel{name = <<"geoloc">>,
           attrs = [{<<"xmlns">>, ?NS_GEOLOC}],
           children = tl(geoloc_data(Data))}.

end_geoloc_item() ->
    #xmlel{name = <<"geoloc">>,
           attrs = [{<<"xmlns">>, ?NS_GEOLOC}],
           children = []}.

geoloc_data(loc_test) ->
    [cdata_item(<<"lat">>, <<"6.789">>),
     cdata_item(<<"lon">>, <<"-77">>),
     cdata_item(<<"accuracy">>, <<"1.23">>)];

geoloc_data(bot_test) ->
    [cdata_item(<<"lat">>, float_to_binary(?BOT_LAT)),
     cdata_item(<<"lon">>, float_to_binary(?BOT_LON)),
     cdata_item(<<"accuracy">>, <<"1.00">>)].

cdata_item(Name, Val) ->
    #xmlel{name = Name,
           children = [#xmlcdata{content = Val}]}.

insert_bot(Client) ->
    Jid = escalus_client:short_jid(Client),
    'Elixir.Wocky.Factory':insert(bot, [{owner, Jid},
                                        {lat, 6.789},
                                        {lon, -77}]).
