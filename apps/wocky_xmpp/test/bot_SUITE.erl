%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite for mod_wocky_bot
-module(bot_SUITE).

-compile(export_all).
-compile({parse_transform, fun_chain}).
-compile({parse_transform, cut}).
-compile({parse_transform, do}).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("test_helper.hrl").
-include("wocky_bot.hrl").

-import(test_helper, [expect_iq_success/2, expect_iq_error/2,
                      rsm_elem/1, decode_rsm/1, check_rsm/5,
                      get_hs_stanza/0, bot_node/1,
                      check_hs_result/4, expect_iq_success_u/3,
                      publish_item_stanza/4, publish_item_stanza/5,
                      retract_item_stanza/2, subscribe_stanza/0,
                      node_el/2, node_el/3, cdata_el/2,
                      ensure_all_clean/1, hs_query_el/1, hs_node/1,
                      add_to_s/2, set_notifications/2,
                      check_home_stream_sizes/2,
                      check_home_stream_sizes/3
                     ]).

-export([set_visibility/3, create_field/1]).

-define(BOT_TITLE, <<"Alice's Bot">>).
-define(BOT_NAME, <<"AliceBot">>).
-define(BOT_DESC, <<"A test bot owned by Alice">>).
-define(BOT_ADDRESS, <<"260 Tinakori Road, Thorndon, Wellington">>).
-define(BOT_TYPE, <<"LucyLiuBot">>).
-define(BOT_LAT, 55.0).
-define(BOT_LON, 60.1).
-define(BOT_RADIUS, 10000).

-define(CREATE_TITLE,       <<"Created Bot">>).
-define(CREATE_SHORTNAME,   <<"NewBot">>).
-define(CREATE_DESCRIPTION, <<"Test bot for creation operation">>).
-define(CREATE_ADDRESS,     <<"5 Adelaide Avenue, Deakin, ACT">>).
-define(CREATE_LOCATION,    {2.5, 1.6}).
-define(CREATE_RADIUS,      10).
-define(CREATE_IMAGE,       <<"tros:localhost/file/123465">>).
-define(CREATE_TYPE,        <<"floatbot">>).
-define(CREATE_TAGS,        [<<"tag1">>, <<"tag2">>]).

-define(NEW_DESCRIPTION,    <<"New bot description!">>).

-define(CREATED_BOTS,       30).
-define(CREATED_ITEMS,      50).


%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     create,
     new_id,
     retrieve,
     update,
     subscribers,
     unsubscribe,
     subscribe,
     subscribe_temporary,
     unsubscribe_temporary,
     delete,
     errors,
     retrieve_for_user,
     get_subscribed,
     publish_item,
     retract_item,
     edit_item,
     get_items,
     publish_image_item,
     item_images,
     follow_me,
     unfollow_me,
     share,
     share_multicast,
     open_visibility,
     follow_notifications,
     geosearch,
     empty_shortname
    ].

suite() ->
    escalus:suite().


%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),
    reset_tables(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_testcase(geosearch, Config) ->
    meck:new('Elixir.Wocky.Index', [passthrough]),
    meck:expect('Elixir.Wocky.Index', geosearch,
                fun (_Lat, _Lon) ->
                        {ok, [#{id => ?BOT, server => ?SERVER,
                                user_id => ?ALICE, title => ?BOT_TITLE,
                                image => ?CREATE_IMAGE, lat => ?BOT_LAT,
                                lon => ?BOT_LON, radius => ?BOT_RADIUS,
                                distance => 8000}]}
                end),
    escalus:init_per_testcase(geosearch, Config);
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(geosearch, Config) ->
    meck:unload(),
    escalus:end_per_testcase(geosearch, Config);
end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

local_tables() ->
    [bot_name, bot_item, home_stream].

reset_tables(Config) ->
    ?wocky_repo:delete_all(?wocky_user),
    Config2 = fun_chain:first(Config,
        escalus:init_per_suite(),
        test_helper:setup_users([alice, bob, carol, karen, robert, tim])
    ),

    Alice = ?wocky_repo:get(?wocky_user, ?ALICE),
    Bob = ?wocky_repo:get(?wocky_user, ?BOB),
    Carol = ?wocky_repo:get(?wocky_user, ?CAROL),
    Karen = ?wocky_repo:get(?wocky_user, ?KAREN),
    Robert = ?wocky_repo:get(?wocky_user, ?ROBERT),

    ?wocky_factory:insert(roster_item, #{user => Alice, contact => Bob}),
    ?wocky_factory:insert(roster_item, #{user => Alice, contact => Carol}),
    ?wocky_factory:insert(roster_item, #{user => Alice, contact => Robert}),
    ?wocky_factory:insert(roster_item, #{user => Alice, contact => Karen}),

    Bot = ?wocky_factory:insert(bot, #{id => ?BOT,
                                       title => ?BOT_TITLE,
                                       shortname => ?BOT_NAME,
                                       user => Alice,
                                       description => ?BOT_DESC,
                                       lat => ?BOT_LAT,
                                       lon => ?BOT_LON,
                                       radius => ?BOT_RADIUS,
                                       address => ?BOT_ADDRESS,
                                       image => ?AVATAR_FILE,
                                       type => ?BOT_TYPE}),

    ?wocky_item:put(Bot, ?ITEM, ?ITEM_STANZA, true),
    ?wocky_item:put(Bot, ?ITEM2, ?ITEM_STANZA2, false),

    ?wocky_share:put(Bob, Bot, Alice),
    ?wocky_subscription:put(Carol, Bot),
    ?wocky_subscription:put(Karen, Bot),

    Config2.

%%--------------------------------------------------------------------
%% mod_wocky_bot tests
%%--------------------------------------------------------------------

create(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        % Successfully create a bot
        Stanza = expect_iq_success(create_stanza(), Alice),
        check_returned_bot(Stanza, expected_create_fields()),

        % No home stream updates should occur for private bots
        timer:sleep(400),
        check_home_stream_sizes(0, [Alice, Bob]),

        CreateFields = [{"public", "bool", true} |
                        lists:keydelete("shortname", 1, default_fields())],
        expect_iq_success(create_stanza(CreateFields), Alice),

        % Both the creator and their friends and followers should get
        % HS notifications for public bots:
        timer:sleep(400),
        check_home_stream_sizes(1, [Alice, Bob], false),

        % Fail due to shortname conflict if we try to create the same bot
        expect_iq_error(create_stanza(), Alice)
      end).

new_id(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        %% Get a new ID
        Result = expect_iq_success(new_id_stanza(), Alice),
        ID = xml:get_path_s(Result, [{elem, <<"new-id">>}, cdata]),

        %% We can't specify an un-allocated ID for creation
        CreateFields = [{"id", "string", ID} |
                        lists:keydelete("shortname", 1, default_fields())],
        expect_iq_error(create_stanza(CreateFields), Bob),
        expect_iq_success(create_stanza(CreateFields), Alice),

        %% Alice can publish to the bot ID
        publish_item(ID, <<"ID">>,
                     <<"title">>, <<"content">>, undefined, Alice),

        %% Bob can't since he's not the owner
        expect_iq_error(
          publish_item_stanza(ID, <<"ID">>, <<"title">>, <<"content">>), Bob),

        % Now create the bot
        CreateFields = [{"id", "string", ID} |
                        lists:keydelete("shortname", 1, default_fields())],
        expect_iq_success(create_stanza(CreateFields), Alice),

        %% We can't specify an un-allocated ID for creation
        FailedCreateFields = [{"id", "string", ?wocky_id:new()}
                              | default_fields()],
        expect_iq_error(create_stanza(FailedCreateFields), Alice)
      end).

retrieve(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}],
      fun(Alice, Bob, Carol) ->
        % Alice can retrieve her own bot
        Stanza = expect_iq_success(retrieve_stanza(), Alice),
        check_returned_bot(Stanza, expected_retrieve_fields(true)),

        % Bob can retrieve the bot since it's shared to him
        Stanza2 = expect_iq_success(retrieve_stanza(), Bob),
        check_returned_bot(Stanza2, expected_retrieve_fields(false)),

        % Carol cannot retrive since the bot is not public
        expect_iq_error(retrieve_stanza(), Carol)
      end).

update(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        % Alice can update the bot
        expect_iq_success(update_stanza(), Alice),

        % And the new bot should have the change
        Stanza = expect_iq_success(retrieve_stanza(), Alice),
        NewFields =
            lists:keyreplace("description", 1, expected_retrieve_fields(true),
                            {"description", string, ?NEW_DESCRIPTION}),
        check_returned_bot(Stanza, NewFields),

        % Bob can't update it since he's not the owner
        expect_iq_error(update_stanza(), Bob)
      end).

subscribers(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        % Alice can get the correct subscribers
        Stanza = expect_iq_success(subscribers_stanza(), Alice),
        check_subscribers(Stanza, [?BJID(?CAROL),
                                   ?BJID(?KAREN)]),

        % Bob can't because he's not the owner
        expect_iq_error(subscribers_stanza(), Bob)
      end).

unsubscribe(Config) ->
    escalus:story(Config, [{alice, 1}, {carol, 1}],
      fun(Alice, Carol) ->
        Stanza1 = expect_iq_success(unsubscribe_stanza(), Carol),
        check_subscriber_count(Stanza1, 2),

        % Alice can get the correct subscribers
        Stanza2 = expect_iq_success(subscribers_stanza(), Alice),
        check_subscribers(Stanza2, [?BJID(?KAREN)])
      end).

subscribe(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}],
      fun(Alice, Bob, Carol) ->
        set_visibility(Alice, ?WOCKY_BOT_VIS_OPEN, ?BOT),

        check_returned_bot(expect_iq_success(retrieve_stanza(), Carol),
                           expected_retrieve_fields(false, ?NEW_DESCRIPTION,
                                                    ?WOCKY_BOT_VIS_OPEN, 1)),
        Stanza1 = expect_iq_success(subscribe_stanza(), Carol),
        check_returned_bot(expect_iq_success(retrieve_stanza(), Carol),
                           expected_retrieve_fields(true, ?NEW_DESCRIPTION,
                                                    ?WOCKY_BOT_VIS_OPEN, 2)),
        check_subscriber_count(Stanza1, 3),

        Stanza2 = expect_iq_success(subscribe_stanza(), Bob),
        check_subscriber_count(Stanza2, 4),

        % Alice can get the correct subscribers
        Stanza3 = expect_iq_success(subscribers_stanza(), Alice),
        check_subscribers(Stanza3, [?BJID(?KAREN),
                                    ?BJID(?CAROL),
                                    ?BJID(?BOB)])
      end).

subscribe_temporary(Config) ->
    reset_tables(Config),
    escalus:story(Config, [{alice, 1}, {tim, 1}],
      fun(Alice, Tim) ->
        set_visibility(Alice, ?WOCKY_BOT_VIS_OPEN, ?BOT),

        check_returned_bot(expect_iq_success(retrieve_stanza(), Tim),
                           expected_retrieve_fields(false, ?BOT_DESC,
                                                    ?WOCKY_BOT_VIS_OPEN, 2)),
        subscribe_temporary(?BOT_B_JID, Tim),

        check_returned_bot(expect_iq_success(retrieve_stanza(), Tim),
                           expected_retrieve_fields(true, ?BOT_DESC,
                                                    ?WOCKY_BOT_VIS_OPEN, 3)),

        Stanza3 = expect_iq_success(subscribers_stanza(), Alice),
        check_subscribers(Stanza3, [?BJID(?CAROL), ?BJID(?KAREN),
                                    escalus_client:full_jid(Tim)]),

        ensure_all_clean([Alice, Tim])
      end).

unsubscribe_temporary(Config) ->
    escalus:story(Config, [{alice, 1}, {tim, 1}],
      fun(Alice, Tim) ->
        set_visibility(Alice, ?WOCKY_BOT_VIS_OPEN, ?BOT),

        %% Tim's previous temp subscription should have been cleared
        %% by his disconnection
        Stanza = expect_iq_success(subscribers_stanza(), Alice),
        check_subscribers(Stanza, [?BJID(?CAROL), ?BJID(?KAREN)]),

        subscribe_temporary(?BOT_B_JID, Tim),

        Stanza2 = expect_iq_success(subscribers_stanza(), Alice),
        check_subscribers(Stanza2, [?BJID(?CAROL), ?BJID(?KAREN),
                                    escalus_client:full_jid(Tim)]),

        unsubscribe_temporary(?BOT_B_JID, Tim),

        Stanza3 = expect_iq_success(subscribers_stanza(), Alice),
        check_subscribers(Stanza3, [?BJID(?CAROL), ?BJID(?KAREN)]),
        ensure_all_clean([Alice, Tim])
      end).

delete(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        expect_iq_error(delete_stanza(), Bob),
        expect_iq_success(retrieve_stanza(), Alice),

        expect_iq_success(delete_stanza(), Alice),
        expect_iq_error(retrieve_stanza(), Alice)
      end).


errors(Config) ->
    escalus:story(Config, [{alice, 1}],
      fun(Alice) ->
        Missing = lists:keydelete("title", 1, default_fields()),
        expect_iq_error(create_stanza(Missing), Alice),

        Extra = [{"unknownfield", "string", <<"abc">>} | default_fields()],
        expect_iq_error(create_stanza(Extra), Alice),

        WrongType = [{"title", "int", 10} | Missing],
        expect_iq_error(create_stanza(WrongType), Alice)
      end).

retrieve_for_user(Config) ->
    reset_tables(Config),
    ?wocky_repo:delete_all(?wocky_bot),
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        IDs = [create_simple_bot(Alice) || _ <- lists:seq(1, ?CREATED_BOTS)],
        {_PrivateBots, PublicBots} = distribute(IDs),

        set_visibility(Alice, ?WOCKY_BOT_VIS_OPEN, PublicBots),

        %% Alice can see all her bots
        Stanza = expect_iq_success(
                   retrieve_stanza(?BJID(?ALICE), #rsm_in{}), Alice),
        check_returned_bots(Stanza, IDs, 0, ?CREATED_BOTS),

        Stanza2 = expect_iq_success(
                   retrieve_stanza(?BJID(?ALICE),
                                   #rsm_in{direction = before}), Alice),
        check_returned_bots(Stanza2, IDs, 0, ?CREATED_BOTS),

        %% Bob can only see the public bots
        Stanza3 = expect_iq_success(
                    retrieve_stanza(?BJID(?ALICE), #rsm_in{}), Bob),
        check_returned_bots(Stanza3, PublicBots,
                            0, length(PublicBots)),

        %% Test some basic RSM functionality
        %% Bob can only see the subset of bots set to be visible by everyone
        Stanza6 = expect_iq_success(
                    retrieve_stanza(?BJID(?ALICE),
                                    #rsm_in{index = 3, max = 2}), Bob),
        ExpectedBots = lists:sublist(PublicBots, 4, 2),
        check_returned_bots(Stanza6, ExpectedBots, 3,
                            length(PublicBots)),

        %% Test RSM id functionality
        %% Bob can only see the subset of bots set to be visible by everyone
        Stanza7 = expect_iq_success(
                    retrieve_stanza(?BJID(?ALICE),
                                    #rsm_in{id = hd(tl(PublicBots)),
                                            max = 2,
                                            direction = aft
                                           }),
                    Bob),
        ExpectedBots2 = lists:sublist(PublicBots, 3, 2),
        check_returned_bots(Stanza7, ExpectedBots2, 2,
                            length(PublicBots)),

        %% When alice publishes to a bot, that bot should become the most
        %% recently updated, moving it to the end of the list:
        PublishBot = lists:nth(10, PublicBots),
        NoteID = <<"Note">>,
        Title = <<"Title">>,
        Content = <<"Content">>,
        publish_item(PublishBot, NoteID, Title, Content, undefined, Alice),

        %% Bob can see all the public bots with the updated one now at the end
        Stanza8 = expect_iq_success(
                   retrieve_stanza(?BJID(?ALICE), #rsm_in{}), Bob),
        Bots = check_returned_bots(Stanza8, PublicBots, 0, ?CREATED_BOTS div 2),
        check_returned_bot(
          lists:last(Bots -- [lists:last(Bots)]),
          [{"id",          string, PublishBot},
           {"title",       string, ?CREATE_TITLE},
           {"description", string, ?CREATE_DESCRIPTION},
           {"address",     string, ?CREATE_ADDRESS},
           {"location",    geoloc, ?CREATE_LOCATION},
           {"radius",      int,    ?CREATE_RADIUS},
           {"image",       string, ?CREATE_IMAGE},
           {"type",        string, ?CREATE_TYPE},
           {"owner",       jid,    ?BJID(?ALICE)},
           {"visibility",  int,    ?WOCKY_BOT_VIS_OPEN},
           {"alerts",      int,    ?WOCKY_BOT_ALERT_DISABLED},
           {"jid",         jid,    bot_jid(PublishBot)},
           {"subscribed",  bool,   false}])
      end).

get_subscribed(Config) ->
    reset_tables(Config),
    escalus:story(Config, [{alice, 1}, {bob, 1}, {karen, 1}],
      fun(Alice, Bob, Karen) ->
        set_visibility(Alice, ?WOCKY_BOT_VIS_OPEN, ?BOT),

        %% Alice is the owner (and therefore a follower) so should get the bot
        Stanza = expect_iq_success(subscribed_stanza(#rsm_in{}), Alice),
        check_returned_bots(Stanza, [?BOT], 0, 1),

        %% Karen is a subscriber so should get the bot
        Stanza2 = expect_iq_success(subscribed_stanza(#rsm_in{}), Karen),
        check_returned_bots(Stanza2, [?BOT], 0, 1),

        %% Bob is not subscribed to the bot at all so gets nothing
        Stanza3 = expect_iq_success(subscribed_stanza(#rsm_in{}), Bob),
        check_returned_bots(Stanza3, [], undefined, 0)
      end).

publish_item(Config) ->
    reset_tables(Config),
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}, {karen, 1}],
      fun(Alice, Bob, Carol, Karen) ->
        NoteID = <<"item1">>,
        Content = <<"content ZZZ">>,
        Title = <<"title ZZZ">>,
        % Alice publishes an item to her bot
        publish_item(?BOT, NoteID, Title, Content, undefined, Alice),

        % Carol and Karen are subscribers, and so receive a notification
        % Bob is an affiliate but not subscribed, so does not receive anything
        expect_item_publication(Carol, ?BOT, NoteID, Title, Content),
        expect_item_publication(Karen, ?BOT, NoteID, Title, Content),

        % As the owner, Alice should *not* get a notification,
        % so her HS should be empty
        Stanza = expect_iq_success_u(get_hs_stanza(), Alice, Alice),
        check_hs_result(Stanza, 0, 0, false),

        % Nobody else can publish an item to the bot besides the owner
        expect_iq_error(
          publish_item_stanza(?BOT, NoteID, Title, Content),
          Bob),
        expect_iq_error(
          publish_item_stanza(?BOT, NoteID, Title, Content),
          Carol),

        % Alice cannot publish to a non-existant bot
        expect_iq_error(
          publish_item_stanza(?wocky_id:new(), NoteID, Title, Content), Alice),

        test_helper:ensure_all_clean([Alice, Bob, Carol, Karen])
      end).

retract_item(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}, {karen, 1}],
      fun(Alice, Bob, Carol, Karen) ->
        % Nobody else can publish an item to the bot besides the owner
        expect_iq_error(
          retract_item_stanza(?BOT, ?ITEM),
          Bob),
        expect_iq_error(
          retract_item_stanza(?BOT, ?ITEM),
          Carol),

        % Alice can retract the item as its owner
        expect_iq_success(retract_item_stanza(?BOT, ?ITEM), Alice),

        % Carol and Karen are subscribers, and so receive a notification
        % Bob is an affiliate but not subscribed, so does not receive anything
        expect_item_retraction(Carol, ?BOT, ?ITEM),
        expect_item_retraction(Karen, ?BOT, ?ITEM),

        test_helper:ensure_all_clean([Alice, Bob, Carol, Karen])
      end).

edit_item(Config) ->
    reset_tables(Config),
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}, {karen, 1}],
      fun(Alice, Bob, Carol, Karen) ->
        NoteID = ?ITEM,
        Content = <<"updated content">>,
        Title = <<"updated title">>,
        % Alice updates an item on her bot
        publish_item(?BOT, NoteID, Title, Content, undefined, Alice),

        % Carol and Karen are subscribers, and so receive a notification
        % Bob is an affiliate but not subscribed, so does not receive anything
        expect_item_publication(Carol, ?BOT, NoteID, Title, Content),
        expect_item_publication(Karen, ?BOT, NoteID, Title, Content),

        % Nobody else can edit an item to the bot besides the owner
        expect_iq_error(
          publish_item_stanza(?BOT, NoteID, Title, Content),
          Bob),
        expect_iq_error(
          publish_item_stanza(?BOT, NoteID, Title, Content),
          Carol),

        test_helper:ensure_all_clean([Alice, Bob, Carol, Karen])
      end).

get_items(Config) ->
    ?wocky_repo:delete_all(?wocky_item),
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}, {karen, 1}],
      fun(Alice, Bob, Carol, Karen) ->
        % Alice publishes a bunch of items on her bot
        lists:foreach(
          add_item(Alice, [Carol, Karen], _), lists:seq(0, ?CREATED_ITEMS-1)),

        % Bob can get items because he has the bot shared to him
        get_items(Bob, #rsm_in{max = 10}, 0, 9),
        get_items(Bob, #rsm_in{index = 5},
                  5, ?CREATED_ITEMS-1),
        get_items(Bob, #rsm_in{max = 2, direction = before, id = item_id(5)},
                  3, 4),
        get_items(Bob, #rsm_in{max = 3, direction = aft, id = item_id(48)},
                  49, min(?CREATED_ITEMS-1, 51)),
        get_items(Bob, #rsm_in{max = 3, direction = before},
                  ?CREATED_ITEMS-3, ?CREATED_ITEMS-1),

        % Carol can't because she hasn't
        expect_iq_error(
               test_helper:iq_get(?NS_BOT, query_el(#rsm_in{max = 10})), Carol),

        test_helper:ensure_all_clean([Alice, Bob, Carol, Karen])
      end).

publish_image_item(Config) ->
    reset_tables(Config),
    escalus:story(Config, [{alice, 1}],
      fun(Alice) ->
        NoteID = <<"new-item1">>,
        Content = <<"Some content">>,
        Image = <<"MyImage.jpg">>,
        Title = <<"title ZZZ">>,
        % Alice publishes an item to her bot
        publish_item(?BOT, NoteID, Title, Content, Image, Alice),

        Expected =
        lists:keyreplace("image_items", 1, expected_retrieve_fields(true),
                         {"image_items", int, 2}),
        Stanza = expect_iq_success(retrieve_stanza(), Alice),
        check_returned_bot(Stanza, Expected),

        test_helper:ensure_all_clean([Alice])
      end).

item_images(Config) ->
    reset_tables(Config),
    escalus:story(Config, [{alice, 1}, {bob, 1}, {tim, 1}],
      fun(Alice, Bob, Tim) ->
        lists:foreach(publish_item_with_image(_, Alice), lists:seq(0, 9)),

        %% Alice can see all the images
        Stanza = expect_iq_success(item_image_stanza(), Alice),
        check_returned_images(Stanza, 0, 9),

        %% So can bob who can access the bot
        Stanza2 = expect_iq_success(item_image_stanza(), Bob),
        check_returned_images(Stanza2, 0, 9),

        %% Tim cannot since he can't see the bot
        expect_iq_error(item_image_stanza(), Tim)
      end).

follow_me(Config) ->
    reset_tables(Config),
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        %% Alice can set a bot to follow her
        expect_iq_success(follow_me_stanza(), Alice),

        %% Bob cannot since he is not the bot owner
        expect_iq_error(follow_me_stanza(), Bob)
      end).

unfollow_me(Config) ->
    reset_tables(Config),
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        %% Alice can unfollow-me a bot that is following her
        expect_iq_success(follow_me_stanza(), Alice),
        expect_iq_success(unfollow_me_stanza(), Alice),

        %% Alice can unfollow-me a bot that is not following her
        expect_iq_success(unfollow_me_stanza(), Alice),

        %% Bob cannot unfollow-me a bot that he doesn't own
        expect_iq_error(unfollow_me_stanza(), Bob)
      end).

follow_notifications(Config) ->
    reset_tables(Config),
    escalus:story(Config, [{alice, 1}],
      fun(Alice) ->
        %% Subscribe to HS notifications
        escalus:send(Alice,
            escalus_stanza:presence_direct(hs_node(?ALICE), <<"available">>,
                                           [hs_query_el(undefined)])),

        %% Simple follow on and off tests
        escalus:send(Alice, test_helper:add_to_s(follow_me_stanza(), Alice)),
        escalus:assert_many(
          [is_bot_action_hs_notification(?BOT, <<"follow on">>, _),
           is_iq_result],
          escalus:wait_for_stanzas(Alice, 2)),

        escalus:send(Alice, test_helper:add_to_s(unfollow_me_stanza(), Alice)),
        escalus:assert_many(
          [is_bot_action_hs_notification(?BOT, <<"follow off">>, _),
           is_iq_result],
          escalus:wait_for_stanzas(Alice, 2)),

        wocky_bot_expiry_mon:set_warning_time(2),

        %% Test the expiry notifications
        escalus:send(Alice, test_helper:add_to_s(follow_me_stanza(5), Alice)),
        escalus:assert_many(
          [is_bot_action_hs_notification(?BOT, <<"follow on">>, _),
           is_iq_result],
          escalus:wait_for_stanzas(Alice, 2)),

        escalus:assert(
          is_bot_action_hs_notification(?BOT, <<"follow expire">>, _),
          escalus:wait_for_stanza(Alice, 4000)),

        escalus:assert(
          is_bot_action_hs_notification(?BOT, <<"follow expire">>, _),
          escalus:wait_for_stanza(Alice, 2500)),

        %% Test reset of expiry
        escalus:send(Alice, test_helper:add_to_s(follow_me_stanza(5), Alice)),
        escalus:assert_many(
          [is_bot_action_hs_notification(?BOT, <<"follow on">>, _),
           is_iq_result],
          escalus:wait_for_stanzas(Alice, 2)),

        escalus:assert(
          is_bot_action_hs_notification(?BOT, <<"follow expire">>, _),
          escalus:wait_for_stanza(Alice, 4000)),

        escalus:send(Alice, test_helper:add_to_s(follow_me_stanza(5), Alice)),
        escalus:assert_many(
          [is_bot_action_hs_notification(?BOT, <<"follow on">>, _),
           is_iq_result],
          escalus:wait_for_stanzas(Alice, 2)),

        escalus:assert(
          is_bot_action_hs_notification(?BOT, <<"follow expire">>, _),
          escalus:wait_for_stanza(Alice, 4000)),

        escalus:assert(
          is_bot_action_hs_notification(?BOT, <<"follow expire">>, _),
          escalus:wait_for_stanza(Alice, 2500)),

        Stanza = expect_iq_success_u(test_helper:get_hs_stanza(), Alice, Alice),
        test_helper:check_hs_result(Stanza, 3)
      end).

share(Config) ->
    reset_tables(Config),
    escalus:story(Config, [{alice, 1}, {tim, 1}],
      fun(Alice, Tim) ->
        set_visibility(Alice, ?WOCKY_BOT_VIS_FRIENDS, [?BOT]),

        set_notifications(true, Tim),

        % Tim can't see the private bot because it's not shared to him
        expect_iq_error(retrieve_stanza(), Tim),

        [] = list_notifications(),

        % Alice shares the bot to him
        escalus:send(Alice, share_stanza(?BOT, Alice, Tim)),
        timer:sleep(500),

        % Tim can now see the bot
        expect_iq_success(retrieve_stanza(), Tim),

        1 = length(list_notifications()),

        test_helper:ensure_all_clean([Alice, Tim])
      end).

share_multicast(Config) ->
    reset_tables(Config),
    clear_notifications(),
    escalus:story(Config, [{alice, 1}, {tim, 1}],
      fun(Alice, Tim) ->
        set_visibility(Alice, ?WOCKY_BOT_VIS_FRIENDS, [?BOT]),

        set_notifications(true, Tim),

        % Alice shares the bot to him
        Stanza = multicast_SUITE:multicast_stanza(
                   [[{<<"type">>, <<"to">>},
                     {<<"jid">>, ?BJID(?TIM)}]],
                   ?NS_ADDRESS,
                   escalus_stanza:to(share_stanza(?BOT), ?SERVER)
                  ),
        escalus:send(Alice, Stanza),
        timer:sleep(500),

        % Tim can now see the bot
        expect_iq_success(retrieve_stanza(), Tim),

        1 = length(list_notifications()),

        test_helper:ensure_all_clean([Alice, Tim])
      end).


open_visibility(Config) ->
    reset_tables(Config),
    ?wocky_repo:delete_all(?wocky_roster_item),
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}, {tim, 1}],
      fun(Alice, Bob, Carol, Tim) ->
        set_visibility(Alice, ?WOCKY_BOT_VIS_OPEN, [?BOT]),

        % Everyone can see the bot because it's public
        expect_iq_success(retrieve_stanza(), Bob),
        expect_iq_success(retrieve_stanza(), Carol),
        expect_iq_success(retrieve_stanza(), Tim),

        test_helper:ensure_all_clean([Alice, Bob, Carol, Tim])
      end).

geosearch(Config) ->
    reset_tables(Config),
    escalus:story(Config, [{alice, 1}],
      fun(Alice) ->
        Stanza = expect_iq_success(add_to_s(geosearch_stanza(), Alice), Alice),
        check_geosearch_return(Stanza)
      end).

empty_shortname(Config) ->
    escalus:story(Config, [{alice, 1}],
      fun(Alice) ->
        % Successfully create a bot with an empty (but present) shortname field
        Fields = lists:keyreplace("shortname", 1, default_fields(),
                                  {"shortname", "string", ""}),
        expect_iq_success(create_stanza(Fields), Alice)
      end).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

clear_notifications() ->
    'Elixir.Pushex.Sandbox':clear_notifications([{pid, notifier()}]).

list_notifications() ->
    'Elixir.Pushex.Sandbox':wait_notifications([{count, 1}, {pid, notifier()}]).

notifier() ->
    whereis(push_notification_event_handler).

new_id_stanza() ->
    test_helper:iq_set(?NS_BOT, #xmlel{name = <<"new-id">>}).

create_stanza() ->
    create_stanza(default_fields()).
create_stanza(Fields) ->
    test_helper:iq_set(?NS_BOT,
                       #xmlel{name = <<"create">>,
                              children = create_fields(Fields)}).

default_fields() ->
    [{"title",         "string", ?CREATE_TITLE},
     {"shortname",     "string", ?CREATE_SHORTNAME},
     {"description",   "string", ?CREATE_DESCRIPTION},
     {"address",       "string", ?CREATE_ADDRESS},
     {"location",      "geoloc", ?CREATE_LOCATION},
     {"radius",        "int",    ?CREATE_RADIUS},
     {"image",         "string", ?CREATE_IMAGE},
     {"type",          "string", ?CREATE_TYPE},
     {"tags",          "tags",   ?CREATE_TAGS}
    ].

create_fields(Fields) ->
    [create_field(F) || F <- Fields].

create_field({Name, "string", Value}) ->
    create_field(Name, "string", value_element(Value));
create_field({Name, "int", Value}) ->
    create_field(Name, "int", value_element(integer_to_binary(Value)));
create_field({Name, "jid", Value}) ->
    create_field(Name, "jid", value_element(jid:to_binary(Value)));
create_field({Name, "geoloc", Value}) ->
    create_field(Name, "geoloc", geoloc_element(Value));
create_field({Name, "tags", Values}) ->
    create_field(Name, "tags",
                 [wocky_xml:cdata_el(<<"tag">>, V) || V <- Values]);
create_field({Name, "bool", Value}) ->
    create_field(Name, "bool", value_element(atom_to_binary(Value, utf8))).

value_element(Value) ->
    wocky_xml:cdata_el(<<"value">>, Value).

geoloc_element({Lat, Lon}) ->
    #xmlel{name = <<"geoloc">>,
           attrs = [{<<"xmlns">>, ?NS_GEOLOC}],
           children = [coordinate_element(<<"lat">>, Lat),
                       coordinate_element(<<"lon">>, Lon)]}.

coordinate_element(Name, Val) ->
    #xmlel{name = Name,
           children = [#xmlcdata{content = float_to_binary(Val)}]}.

create_field(Name, Type, Children) when is_list(Children) ->
    #xmlel{name = <<"field">>,
           attrs = [{<<"var">>, list_to_binary(Name)},
                    {<<"type">>, list_to_binary(Type)}],
           children = Children};
create_field(Name, Type, Child) ->
    create_field(Name, Type, [Child]).

expected_create_fields() ->
    [{"id",                 string, any},
     {"server",             string, ?SERVER},
     {"title",              string, ?CREATE_TITLE},
     {"shortname",          string, ?CREATE_SHORTNAME},
     {"owner",              jid,    ?BJID(?ALICE)},
     {"description",        string, ?CREATE_DESCRIPTION},
     {"address",            string, ?CREATE_ADDRESS},
     {"image",              string, ?CREATE_IMAGE},
     {"type",               string, ?CREATE_TYPE},
     {"location",           geoloc, ?CREATE_LOCATION},
     {"radius",             int,    ?CREATE_RADIUS},
     {"visibility",         int,    ?WOCKY_BOT_VIS_OWNER},
     {"alerts",             int,    ?WOCKY_BOT_ALERT_DISABLED},
     {"jid",                jid,    any},
     {"image_items",        int,    0},
     {"updated",            timestamp, any},
     {"subscribers+size",   int,    0}, % Owner is always a subscriber
     {"subscribers+hash",   string, any}].

expected_retrieve_fields(Subscribed) ->
    expected_retrieve_fields(Subscribed, ?BOT_DESC,
                             ?WOCKY_BOT_VIS_OWNER, 2).
expected_retrieve_fields(Subscribed, Description, Visibility, Subscribers) ->
    [{"id",                 string, ?BOT},
     {"server",             string, ?SERVER},
     {"title",              string, ?BOT_TITLE},
     {"shortname",          string, ?BOT_NAME},
     {"owner",              jid,    ?BJID(?ALICE)},
     {"description",        string, Description},
     {"address",            string, ?BOT_ADDRESS},
     {"image",              string, ?AVATAR_FILE},
     {"type",               string, ?BOT_TYPE},
     {"location",           geoloc, {?BOT_LAT, ?BOT_LON}},
     {"radius",             int,    ?BOT_RADIUS},
     {"visibility",         int,    Visibility},
     {"alerts",             int,    ?WOCKY_BOT_ALERT_DISABLED},
     {"jid",                jid,    bot_jid(?BOT)},
     {"image_items",        int,    1},
     {"updated",            timestamp, any},
     {"subscribed",         bool,   Subscribed},
     {"subscribers+size",   int,    Subscribers},
     {"subscribers+hash",   string, any}
    ].

expected_geosearch_fields() ->
    [{"jid",                jid,    any},
     {"id",                 string, any},
     {"server",             string, any},
     {"title",              string, any},
     {"image",              string, any},
     {"location",           geoloc, any},
     {"radius",             int,    any},
     {"distance",           int,    any}].

check_returned_bot(#xmlel{name = <<"iq">>, children = [BotStanza]},
                   ExpectedFields) ->
    check_returned_bot(BotStanza, ExpectedFields);

check_returned_bot(#xmlel{name = <<"bot">>, attrs = [{<<"xmlns">>, ?NS_BOT}],
                          children = Children}, ExpectedFields) ->
    check_return_fields(Children, ExpectedFields),
    get_id(Children).

check_returned_bots(#xmlel{name = <<"iq">>, children = [BotsStanza]},
                    ExpectedIDs, Index, Total) ->
    #xmlel{name = <<"bots">>, attrs = [{<<"xmlns">>, ?NS_BOT}],
           children = Children} = BotsStanza,
    ?assertEqual(ok,
       do([error_m ||
           RSM <- check_get_children(Children, <<"set">>,
                                     [{<<"xmlns">>, ?NS_RSM}]),
           RSMOut <- decode_rsm(RSM),
           check_rsm(RSMOut, Total, Index, ExpectedIDs),
           check_ids(ExpectedIDs, Children)
          ])
      ),
    Children.

check_rsm(#rsm_out{count = 0, index = undefined,
                   first = undefined, last = undefined}, 0, undefined, []) ->
    ok;
check_rsm(#rsm_out{count = Count, index = Index, first = F, last = L},
          ExpectedCount, ExpectedIndex, Items) ->
    case {Count, Index, lists:member(F, Items), lists:member(L, Items)} of
        {ExpectedCount, ExpectedIndex, true, true} ->
            ok;
        _Else ->
            {error,
             {bad_rsm, {Count, Index, F, L},
              {ExpectedCount, ExpectedIndex, Items}}}
    end.

check_ids(ExpectedIDs, Children) ->
    IDs = lists:sort(get_ids(Children, [])),
    case lists:sort(ExpectedIDs) of
        IDs -> ok;
        _ -> {error, {incorrect_ids, IDs, ExpectedIDs}}
    end.

get_ids([], Acc) ->
    lists:reverse(Acc);
get_ids([#xmlel{name = <<"bot">>, children = Fields} | Rest], Acc) ->
    get_ids(Rest, [get_id(Fields) | Acc]);
get_ids([_|Rest], Acc) ->
    get_ids(Rest, Acc).

get_id([El = #xmlel{name = <<"field">>, attrs = Attrs} | Rest]) ->
    case xml:get_attr(<<"var">>, Attrs) of
        {value, <<"id">>} -> xml:get_path_s(El, [{elem, <<"value">>}, cdata]);
        _ -> get_id(Rest)
    end.

check_geosearch_return(#xmlel{name = <<"iq">>, children = [BotsStanza]}) ->
    #xmlel{name = <<"bots">>, attrs = [{<<"xmlns">>, ?NS_BOT}],
           children = Children} = BotsStanza,
    lists:foreach(
      fun (#xmlel{name = <<"bot">>, children = Fields}) ->
              check_return_fields(Fields, expected_geosearch_fields())
      end,
      Children).

check_return_fields(Elements, ExpectedFields) ->
    lists:foreach(check_field(_, Elements), ExpectedFields).

check_field({Name0, Type0, any}, Elements) ->
    Name = list_to_binary(Name0),
    Type = atom_to_binary(Type0, utf8),
    case has_field(Name, Type, Elements) of
        true -> ok;
        false ->
            ct:fail("Expected field ~s:~s in elements:~n~p~n",
                    [Name, Type, Elements])
    end;
check_field({Name0, Type0, Value}, Elements) ->
    Name = list_to_binary(Name0),
    Type = atom_to_binary(Type0, utf8),
    case has_field_val(Name, Type, Value, Elements) of
        true -> ok;
        false ->
            ct:fail("Expected field ~s:~s with value ~p in elements:~n~p~n",
                   [Name, Type, Value, Elements])
    end.

has_field(Name, Type, Elements) ->
    find_field(Name, Type, Elements) =/= false.

has_field_val(Name, Type, Value, Elements) ->
    case find_field(Name, Type, Elements) of
        #xmlel{name = <<"field">>, children = ValueEls} ->
            check_value_el(Value, Type, ValueEls);
        X ->
            ct:fail("find_field ~p ~p ~p returned ~p",
                    [Name, Type, Elements, X])
    end.

check_value_el(Value, <<"geoloc">>, [El] = [#xmlel{name = <<"geoloc">>}]) ->
    check_geoloc_val(Value, El);
check_value_el(Values, <<"tags">>, Els) ->
    ExpectedEls = [wocky_xml:cdata_el(<<"tag">>, V) || V <- Values],
    lists:sort(ExpectedEls) =:= liss:sort(Els);
check_value_el(<<>>, Type,
               [#xmlel{name = <<"value">>,
                       children = []}])
    when Type =:= <<"string">> orelse Type =:= <<"jid">> ->
    true;
check_value_el(Value, Type,
               [#xmlel{name = <<"value">>,
                       children = [#xmlcdata{content = Value}]}])
    when Type =:= <<"string">> orelse Type =:= <<"jid">> ->
    true;
check_value_el(Value, <<"int">>,
               [#xmlel{name = <<"value">>,
                       children = [#xmlcdata{content = InValue}]}]) ->
    Value =:= binary_to_integer(InValue);
check_value_el(Value, <<"bool">>,
               [#xmlel{name = <<"value">>,
                       children = [#xmlcdata{content = InValue}]}]) ->
    Value =:= binary_to_atom(InValue, utf8);
check_value_el(Value, Type, Elements) ->
    ct:fail("check_value_el failed: ~p ~p ~p", [Value, Type, Elements]).

check_geoloc_val({Lat, Lon}, #xmlel{name = <<"geoloc">>,
                                    attrs = Attrs,
                                    children = Children}) ->
    ?assertEqual({value, ?NS_GEOLOC}, xml:get_attr(<<"xmlns">>, Attrs)),
    check_child(<<"lat">>, Lat, Children),
    check_child(<<"lon">>, Lon, Children).

check_child(Name, Value, Children) ->
    case lists:keyfind(Name, #xmlel.name, Children) of
        false ->
            ct:fail("Could not find ~p geoloc element", [Name]);
        #xmlel{children = [#xmlcdata{content = InVal}]} ->
            Value == binary_to_float(InVal);
        _ ->
            ct:fail("Wrong value for ~p - expected ~p)", [Name, Value])
    end.

find_field(Name, Type, Elements) ->
    case lists:dropwhile(fun(E) -> not is_field(Name, Type, E) end, Elements) of
        [] -> false;
        List -> hd(List)
    end.

is_field(Name, Type, #xmlel{attrs = Attrs}) ->
    xml:get_attr(<<"var">>, Attrs) =:= {value, Name} andalso
    xml:get_attr(<<"type">>, Attrs) =:= {value, Type}.

retrieve_stanza(User, RSM) ->
    test_helper:iq_get(?NS_BOT,
                       #xmlel{name = <<"bot">>,
                              attrs = [{<<"user">>, User}],
                              children = [rsm_elem(RSM)]}).

retrieve_stanza() ->
    retrieve_stanza(?BOT).

retrieve_stanza(BotID) ->
    test_helper:iq_get(?NS_BOT, node_el(BotID, <<"bot">>)).

subscribed_stanza(RSM) ->
    test_helper:iq_get(?NS_BOT, #xmlel{name = <<"subscribed">>,
                                       children = [rsm_elem(RSM)]}).

bot_jid(ID) ->
    jid:to_binary(jid:make(<<>>, ?SERVER, bot_node(ID))).

change_visibility_stanza(Bot, Visibility) ->
    test_helper:iq_set(?NS_BOT, node_el(Bot, <<"fields">>,
                                        [visibility_field(Visibility)])).

visibility_field(Visibility) ->
    create_field({"visibility", "int", Visibility}).

update_stanza() ->
    test_helper:iq_set(?NS_BOT, node_el(?BOT, <<"fields">>, [modify_field()])).

modify_field() ->
    create_field({"description", "string", ?NEW_DESCRIPTION}).

subscribers_stanza() ->
    test_helper:iq_get(?NS_BOT, node_el(?BOT, <<"subscribers">>)).

check_subscribers(#xmlel{name = <<"iq">>, children = [SubscribersEl]},
                   Subscribers) ->
    ReceivedSubscribers = SubscribersEl#xmlel.children,
    ?assertEqual({value, integer_to_binary(length(Subscribers))},
                 xml:get_attr(<<"size">>, SubscribersEl#xmlel.attrs)),
    ?assertEqual(length(Subscribers), length(ReceivedSubscribers)),
    lists:foreach(check_subscriber(ReceivedSubscribers, _), Subscribers).

check_subscriber(SubscriberEls, Name) ->
    case lists:dropwhile(fun(El) -> not is_subscriber(Name, El) end,
                         SubscriberEls) of
        [] -> ct:fail("Missing subscriber ~p", [Name]);
        _ -> ok
    end.

is_subscriber(Name, #xmlel{name = <<"subscriber">>, attrs = Attrs}) ->
    xml:get_attr(<<"jid">>, Attrs) =:= {value, Name}.

check_subscriber_count(Stanza = #xmlel{name = <<"iq">>}, ExpectedCount) ->
    Count = xml:get_path_s(Stanza, [{elem, <<"subscriber_count">>}, cdata]),
    ?assertEqual(binary_to_integer(Count), ExpectedCount).

is_bot_unsubscribe(#xmlel{name = <<"message">>, children = [Unsubscribed]}) ->
    Attrs = Unsubscribed#xmlel.attrs,
    <<"unsubscribed">> =:= Unsubscribed#xmlel.name andalso
    has_standard_attrs(Attrs);
is_bot_unsubscribe(_) -> false.

has_standard_attrs(Attrs) ->
    {value, bot_node(?BOT)} =:= xml:get_attr(<<"node">>, Attrs)  andalso
    {value, ?NS_BOT} =:= xml:get_attr(<<"xmlns">>, Attrs).

unsubscribe_stanza() ->
    test_helper:iq_set(?NS_BOT, node_el(?BOT, <<"unsubscribe">>)).

delete_stanza() ->
    test_helper:iq_set(?NS_BOT, node_el(?BOT, <<"delete">>)).

expect_item_publication(Client, BotID, NoteID, Title, Content) ->
    S = expect_iq_success_u(get_hs_stanza(), Client, Client),
    I = check_hs_result(S, any, any, false),
    escalus:assert(
      is_publication_update(BotID, NoteID, Title, Content, _),
      hd((lists:last(I))#item.stanzas)).

is_publication_update(BotID, NoteID, Title, Content, Stanza) ->
    R = do([error_m ||
            [Event] <- check_get_children(Stanza, <<"message">>),
            [Item] <- check_get_children(Event, <<"event">>,
                                         [{<<"xmlns">>, ?NS_BOT_EVENT},
                                          {<<"node">>, bot_node(BotID)}]),
            [Entry] <- check_get_children(Item, <<"item">>,
                                          [{<<"id">>, NoteID}]),
            check_get_children(Entry, <<"entry">>, [{<<"xmlns">>, ?NS_ATOM}]),
            check_children_cdata(Entry, [{<<"title">>, Title},
                                         {<<"content">>, Content}]),
            ok
           ]),
    R =:= ok.

check_get_children(Els, Name) -> check_get_children(Els, Name, []).

check_get_children([], Name, _CheckAttrs) -> {error, {el_not_found, Name}};
check_get_children([El | Rest], Name, CheckAttrs) ->
    case check_get_children(El, Name, CheckAttrs) of
        {ok, Children} -> {ok, Children};
        {error, _} -> check_get_children(Rest, Name, CheckAttrs)
    end;

check_get_children(#xmlel{name = Name, attrs = Attrs, children = Children},
                   Name, CheckAttrs) ->
    case lists:all(has_attr(Attrs, _), CheckAttrs) of
        true -> {ok, Children};
        false -> {error, {missing_attr, CheckAttrs}}
    end;
check_get_children(_, Name, Attr) ->
    {error, {no_or_incorrect_element, Name, Attr}}.

check_children_cdata(_Element, []) -> ok;
check_children_cdata(Element, [{Name, Value} | Rest]) ->
    case xml:get_path_s(Element, [{elem, Name}, cdata]) of
        Value -> check_children_cdata(Element, Rest);
        E -> {error, {no_or_incorrect_element, Name, Value, Element, E}}
    end.

has_attr(Attrs, {Name, Val}) ->
    {value, Val} =:= xml:get_attr(Name, Attrs).

expect_item_retraction(Client, BotID, NoteID) ->
    S = expect_iq_success_u(get_hs_stanza(), Client, Client),
    I = check_hs_result(S, any, any, false),
    escalus:assert(
      is_retraction_update(BotID, NoteID, _),
      hd((lists:last(I))#item.stanzas)).

is_retraction_update(BotID, NoteID, Stanza) ->
    R = do([error_m ||
            [Event] <- check_get_children(Stanza, <<"message">>),
            [Item] <- check_get_children(Event, <<"event">>,
                                         [{<<"xmlns">>, ?NS_BOT_EVENT},
                                          {<<"node">>, bot_node(BotID)}]),
            [] <- check_get_children(Item, <<"retract">>,
                                     [{<<"id">>, NoteID}]),
            ok
           ]),
    R =:= ok.

add_item(Client, Subs, N) ->
    NoteID = item_id(N),
    Title = item_title(N),
    Content = item_content(N),
    publish_item(?BOT, NoteID, Title, Content, undefined, Client),

    lists:foreach(
        expect_item_publication(_, ?BOT, NoteID, Title, Content),
        Subs).

get_items(Client, RSM, First, Last) ->
    Result = expect_iq_success(
               test_helper:iq_get(?NS_BOT, query_el(RSM)), Client),

    ?assertEqual(ok, check_result(Result, First, Last)).

query_el(RSM) ->
    #xmlel{name = <<"query">>,
           attrs = [{<<"node">>, bot_node(?BOT)}],
           children = [rsm_elem(RSM)]}.

check_result(Stanza, First, Last) ->
    do([error_m ||
        [Query] <- check_get_children(Stanza, <<"iq">>),
        Children <- check_get_children(Query, <<"query">>,
                                       [{<<"xmlns">>, ?NS_BOT}]),
        RSM <- check_get_children(Children, <<"set">>,
                                  [{<<"xmlns">>, ?NS_RSM}]),
        RSMOut <- decode_rsm(RSM),
        check_rsm(RSMOut, ?CREATED_ITEMS, First, item_id(First), item_id(Last)),
        check_items(Children, First, Last),
        ok
       ]).

item_image_stanza() ->
    QueryEl = node_el(?BOT, <<"item_images">>, [rsm_elem(#rsm_in{})]),
    test_helper:iq_get(?NS_BOT, QueryEl).

check_items(Children, First, Last) ->
    Expected = lists:seq(First, Last),
    check_items(Children, Expected).

% Should be exactly one element left - the RSM set (which we already checked)
check_items([_RSM], []) -> ok;
check_items(Children, []) -> {error, {extra_items, Children}};
check_items(Children, [I | Rest]) ->
    ExpectedLen = length(Children) - 1,
    Remaining = lists:filter(
                  fun(C) -> not is_item(I, C) end,
                  Children),
    case length(Remaining) of
        ExpectedLen -> check_items(Remaining, Rest);
        _ -> {error, {not_found, I}}
    end.

is_item(I, #xmlel{name = <<"item">>,
                  attrs = Attrs,
                  children = [Entry]}) ->
    {value, item_id(I)} =:= xml:get_attr(<<"id">>, Attrs)
    andalso
    is_item_entry(I, Entry);
is_item(_, _) -> false.

is_item_entry(I, El = #xmlel{name = <<"entry">>,
                             attrs = [{<<"xmlns">>, ?NS_ATOM}]}) ->
    item_title(I) =:= xml:get_path_s(El, [{elem, <<"title">>}, cdata])
    andalso
    item_content(I) =:= xml:get_path_s(El, [{elem, <<"content">>}, cdata]);
is_item_entry(_, _) -> false.

item_id(undefined) -> undefined;
item_id(I) when is_binary(I) -> I;
item_id(I) ->
    <<"ID_", (integer_to_binary(I))/binary>>.
item_title(I) ->
    <<"Title_", (integer_to_binary(I))/binary>>.
item_content(I) ->
    <<"Content_", (integer_to_binary(I))/binary>>.
item_image_url(I) ->
    <<"tros:server/Image_", (integer_to_binary(I))/binary>>.

create_simple_bot(Client) ->
    Fields = lists:keydelete("shortname", 1, default_fields()),
    Stanza = expect_iq_success(create_stanza(Fields), Client),
    check_returned_bot(Stanza, expected_simple_bot_fields()).

expected_simple_bot_fields() ->
    lists:keydelete("shortname", 1, expected_create_fields()).

distribute(L) ->
    {A, B} = distribute(L, [], []),
    {lists:reverse(A), lists:reverse(B)}.

distribute([], A, B) ->
    {A, B};
distribute([H], A, B) ->
    {[H|A], B};
distribute([H,H2|T], A, B) ->
    distribute(T, [H|A], [H2|B]).

is_pres_unavailable() ->
    fun(S) ->
            escalus_pred:is_presence_with_type(<<"unavailable">>, S)
    end.

publish_item(BotID, NoteID, Title, Content, Image, Client) ->
    expect_iq_success(publish_item_stanza(BotID, NoteID, Title, Content, Image),
                      Client).

publish_item_with_image(I, Client) ->
    publish_item(?BOT, item_id(I), <<"Title">>, <<"Content">>,
                 item_image_url(I), Client).

check_returned_images(#xmlel{name = <<"iq">>, children = Children},
                      First, Last) ->
    [#xmlel{name = <<"item_images">>, children = ImageList}] = Children,
    Remaining = check_image(?BJID(?ALICE), ?ITEM, ?ITEM_IMAGE, ImageList),
    RSMXML = lists:foldl(
               fun(I, S) ->
                       check_image(?BJID(?ALICE), item_id(I),
                                   item_image_url(I), S)
               end, Remaining, lists:seq(First, Last)),
    {ok, RSMEls} = check_get_children(hd(RSMXML), <<"set">>,
                                      [{<<"xmlns">>, ?NS_RSM}]),
    {ok, RSM} = decode_rsm(RSMEls),
    ok = check_rsm(RSM, (Last - First) + 2, 0, ?ITEM, item_id(Last)).

check_image(Owner, Item, URL,
            [#xmlel{name = <<"image">>, attrs = Attrs} | Rest]) ->
    ?assertEqual({value, Owner}, xml:get_attr(<<"owner">>, Attrs)),
    ?assertEqual({value, Item}, xml:get_attr(<<"item">>, Attrs)),
    ?assertEqual({value, URL}, xml:get_attr(<<"url">>, Attrs)),
    ?assertMatch({value, _}, xml:get_attr(<<"updated">>, Attrs)),
    Rest.

set_visibility(Client, Visibility, BotList) when is_list(BotList) ->
    lists:foreach(
      fun(B) ->
              expect_iq_success(
                change_visibility_stanza(B, Visibility), Client)
      end,
      BotList);
set_visibility(Client, Visibility, Bot) ->
    set_visibility(Client, Visibility, [Bot]).

follow_me_stanza() ->
    follow_me_stanza(86400). % 1 Day
follow_me_stanza(ExpiryPeriod) ->
    Expiry = ?wocky_timestamp:to_string(
                ?timex:add(
                   ?datetime:utc_now(), ?duration:from_seconds(ExpiryPeriod))),
    QueryEl = #xmlel{name = <<"follow-me">>,
                     attrs = [
                       {<<"node">>, bot_node(?BOT)},
                       {<<"expiry">>, Expiry}
                     ]},
    test_helper:iq_set(?NS_BOT, QueryEl).

unfollow_me_stanza() ->
    QueryEl = node_el(?BOT, <<"un-follow-me">>, []),
    test_helper:iq_set(?NS_BOT, QueryEl).

subscribe_temporary(Bot, Client) ->
    Stanza = escalus_stanza:presence_direct(
               Bot, <<>>,
               [#xmlel{name = <<"query">>,
                       attrs = [{<<"xmlns">>, ?NS_BOT}]}]),
    escalus:send(Client, Stanza).

unsubscribe_temporary(Bot, Client) ->
    Stanza = escalus_stanza:presence_direct(Bot, <<"unavailable">>),
    escalus:send(Client, Stanza).

share_stanza(BotID, From, Target) ->
    fun_chain:first(
      BotID,
      share_stanza(),
      escalus_stanza:to(Target),
      escalus_stanza:from(From)
     ).

share_stanza(BotID) ->
    #xmlel{name = <<"message">>,
           attrs = [{<<"type">>, <<"headline">>}],
           children = [cdata_el(<<"body">>, <<"Here's a bot!">>),
                       bot_el(BotID)]}.

bot_el(BotID) ->
    #xmlel{name = <<"bot">>,
           attrs = [{<<"xmlns">>, ?NS_BOT}],
           children = [cdata_el(<<"jid">>, bot_jid(BotID)),
                       cdata_el(<<"id">>, BotID),
                       cdata_el(<<"server">>, ?SERVER),
                       cdata_el(<<"action">>, <<"share">>)]}.

is_bot_action_hs_notification(JID, Action,
                              Stanza = #xmlel{name = <<"message">>}) ->
    case xml:get_path_s(Stanza,
                        [{elem, <<"notification">>},
                         {elem, <<"item">>}, {elem, <<"message">>}]) of
        <<>> -> false;
        SubStanza -> test_helper:is_bot_action(JID, Action, SubStanza)
    end;
is_bot_action_hs_notification(_, _, _) -> false.

geosearch_stanza() ->
    QueryEl = #xmlel{name = <<"bots">>,
                     attrs = [
                       {<<"lat">>, float_to_binary(?BOT_LAT)},
                       {<<"lon">>, float_to_binary(?BOT_LON)}
                     ]},
    test_helper:iq_get(?NS_BOT, QueryEl).
