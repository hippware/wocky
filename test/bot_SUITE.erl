%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite for mod_wocky_bot
-module(bot_SUITE).
-compile(export_all).

-include("wocky.hrl").
-include("wocky_bot.hrl").
-include("wocky_db_seed.hrl").
-include("test_helper.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile({parse_transform, fun_chain}).
-compile({parse_transform, cut}).
-compile({parse_transform, do}).

-export([set_visibility/3]).

-import(test_helper, [expect_iq_success/2, expect_iq_error/2,
                      rsm_elem/1, decode_rsm/1, check_rsm/5,
                      get_hs_stanza/0, bot_node/1,
                      check_hs_result/4, expect_iq_success_u/3,
                      publish_item_stanza/4, publish_item_stanza/5,
                      retract_item_stanza/2, subscribe_stanza/0,
                      node_el/2, node_el/3, cdata_el/2,
                      ensure_all_clean/1, hs_query_el/1, hs_node/1,
                      add_to_s/2
                     ]).

-define(CREATE_TITLE,       <<"Created Bot">>).
-define(CREATE_SHORTNAME,   <<"NewBot">>).
-define(CREATE_DESCRIPTION, <<"Test bot for creation operation">>).
-define(CREATE_ADDRESS,     <<"5 Adelaide Avenue, Deakin, ACT">>).
-define(CREATE_LOCATION,    {2.5, 1.6}).
-define(CREATE_RADIUS,      10).
-define(CREATE_IMAGE,       <<"tros:localhost/file/123465">>).
-define(CREATE_TYPE,        <<"floatbot">>).
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
     affiliations,
     subscribers,
     unsubscribe,
     subscribe,
     subscribe_temporary,
     unsubscribe_temporary,
     temporary_unsubscribe_roster_change,
     delete,
     errors,
     retrieve_for_user,
     get_subscribed,
     update_affiliations,
     friends_only_permissions,
     roster_change_triggers,
     blocked_group,
     delete_owner,
     publish_item,
     retract_item,
     edit_item,
     get_items,
     publish_image_item,
     item_images,
     follow_me,
     unfollow_me,
     share,
     share_open,
     follow_notifications,
     geosearch
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
                                title => ?BOT_TITLE, image => ?CREATE_IMAGE,
                                lat => ?BOT_LAT, lon => ?BOT_LON,
                                radius => ?BOT_RADIUS, distance => 8000}]}
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
    wocky_db:clear_user_tables(?LOCAL_CONTEXT),
    wocky_db:clear_tables(shared, [bot, bot_subscriber, bot_share]),
    wocky_db:clear_tables(?LOCAL_CONTEXT, local_tables()),
    wocky_db_seed:seed_tables(shared, [bot, roster, bot_subscriber]),
    wocky_db_seed:seed_tables(?LOCAL_CONTEXT, local_tables()),
    Users = escalus:get_users([alice, bob, carol, karen, robert, tim]),
    Config1 = fun_chain:first(Config,
        escalus:init_per_suite(),
        escalus:create_users(Users)
    ),
    {ok, _} = wocky_db:query(shared,
                             "UPDATE user SET roster_viewers = roster_viewers "
                             "+ ? WHERE user = ? AND server = ?",
                             #{roster_viewers => [?BOT_B_JID],
                               user => ?ALICE,
                               server => ?LOCAL_CONTEXT},
                             quorum),
    Config1.


%%--------------------------------------------------------------------
%% mod_wocky_bot tests
%%--------------------------------------------------------------------

create(Config) ->
    escalus:story(Config, [{alice, 1}],
      fun(Alice) ->
        % Successfully create a bot
        Stanza = expect_iq_success(create_stanza(), Alice),
        check_returned_bot(Stanza, expected_create_fields()),

        % Fail due to shortname conflict if we try to create the same bot
        expect_iq_error(create_stanza(), Alice)
      end).

new_id(Config) ->
    escalus:story(Config, [{alice, 1}],
      fun(Alice) ->
        %% Get a new ID
        Result = expect_iq_success(new_id_stanza(), Alice),
        ID = xml:get_path_s(Result, [{elem, <<"new-id">>}, cdata]),

        ct:log("ID: ~p\nResult: ~p", [ID, Result]),

        CreateFields = [{"id", "string", ID} |
                        lists:keydelete("shortname", 1, default_fields())],
        expect_iq_success(create_stanza(CreateFields), Alice),

        FailedCreateFields = [{"id", "string", wocky_db:create_id()}
                              | default_fields()],
        expect_iq_error(create_stanza(FailedCreateFields), Alice)
      end).

retrieve(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}],
      fun(Alice, Bob, Carol) ->
        % Alice can retrieve her own bot
        Stanza = expect_iq_success(retrieve_stanza(), Alice),
        check_returned_bot(Stanza, expected_retrieve_fields()),

        % Bob can retrive since he is a spectator
        Stanza2 = expect_iq_success(retrieve_stanza(), Bob),
        check_returned_bot(Stanza2, expected_retrieve_fields()),

        % Carol cannot retrive since the bot's visibiliy is WHITELIST
        % and she is neither an owner nor a spectator
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
        lists:keyreplace("description", 1, expected_retrieve_fields(),
                         {"description", string, ?NEW_DESCRIPTION}),
        check_returned_bot(Stanza, NewFields),

        % Bob can't update it since he's not the owner
        expect_iq_error(update_stanza(), Bob)
      end).

affiliations(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        % Alice can get the correct affiliations
        Stanza = expect_iq_success(affiliations_stanza(), Alice),
        check_affiliations(Stanza, [{?ALICE_B_JID, owner},
                                    {?BOB_B_JID, spectator}]),

        % Bob can't because he's not the owner
        expect_iq_error(affiliations_stanza(), Bob)
      end).

subscribers(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        % Alice can get the correct subscribers
        Stanza = expect_iq_success(subscribers_stanza(), Alice),
        check_subscribers(Stanza, [?ALICE_B_JID,
                                   ?CAROL_B_JID,
                                   ?KAREN_B_JID]),

        % Bob can't because he's not the owner
        expect_iq_error(affiliations_stanza(), Bob)
      end).

unsubscribe(Config) ->
    escalus:story(Config, [{alice, 1}, {carol, 1}],
      fun(Alice, Carol) ->
        Stanza1 = expect_iq_success(unsubscribe_stanza(), Carol),
        check_subscriber_count(Stanza1, 2),

        % Alice can get the correct subscribers
        Stanza2 = expect_iq_success(subscribers_stanza(), Alice),
        check_subscribers(Stanza2, [?ALICE_B_JID,
                                    ?KAREN_B_JID])
      end).

subscribe(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}, {tim, 1}],
      fun(Alice, Bob, Carol, Tim) ->
        test_helper:add_contact(Alice, Tim, [], <<"Timbo">>),
        test_helper:subscribe_pair(Alice, Tim),

        timer:sleep(1000),

        NewAffiliations = [{?CAROL_B_JID, spectator}],
        expect_iq_success(modify_affiliations_stanza(NewAffiliations), Alice),

        escalus:assert(
          is_affiliation_update(escalus_client:short_jid(Carol), spectator, _),
          escalus:wait_for_stanza(Carol)),

        Stanza1 = expect_iq_success(subscribe_stanza(), Carol),
        check_subscriber_count(Stanza1, 3),
        Stanza2 = expect_iq_success(subscribe_stanza(), Bob),
        check_subscriber_count(Stanza2, 4),

        % Tim can't be subscribed because he is not an affiliate and the
        % permission is WHITELIST
        expect_iq_error(subscribe_stanza(), Tim),

        % Alice can get the correct subscribers
        Stanza3 = expect_iq_success(subscribers_stanza(), Alice),
        check_subscribers(Stanza3, [?ALICE_B_JID,
                                   ?KAREN_B_JID,
                                   ?CAROL_B_JID,
                                   ?BOB_B_JID]),

        % Update Carol's follow status
        expect_iq_success(subscribe_stanza(), Carol),
        Stanza4 = expect_iq_success(subscribers_stanza(), Alice),
        check_subscribers(Stanza4, [?ALICE_B_JID,
                                    ?KAREN_B_JID,
                                    ?CAROL_B_JID,
                                    ?BOB_B_JID])
      end).

subscribe_temporary(Config) ->
    reset_tables(Config),
    escalus:story(Config, [{alice, 1}, {tim, 1}],
      fun(Alice, Tim) ->
        set_visibility(Alice, ?WOCKY_BOT_VIS_FRIENDS, ?BOT),

        %% Tim currently can't subscribe because he's in Alice's
        %% __blocked__ group (and therefore is not a friend):
        subscribe_temporary(?BOT_B_JID, Tim),
        timer:sleep(400),

        Stanza = expect_iq_success(subscribers_stanza(), Alice),
        check_subscribers(Stanza, [?ALICE_B_JID, ?CAROL_B_JID, ?KAREN_B_JID]),

        test_helper:subscribe_pair(Alice, Tim),
        test_helper:add_contact(Alice, Tim, <<"unblocked">>, <<"Timmy">>),

        %% But now he can:
        Stanza2 = expect_iq_success(subscribers_stanza(), Alice),
        check_subscribers(Stanza2, [?ALICE_B_JID, ?CAROL_B_JID, ?KAREN_B_JID]),

        subscribe_temporary(?BOT_B_JID, Tim),
        timer:sleep(400),

        Stanza3 = expect_iq_success(subscribers_stanza(), Alice),
        check_subscribers(Stanza3, [?ALICE_B_JID, ?CAROL_B_JID, ?KAREN_B_JID,
                                   escalus_client:full_jid(Tim)
                                  ]),
        ensure_all_clean([Alice, Tim])
      end).

unsubscribe_temporary(Config) ->
    escalus:story([everyone_is_friends | Config], [{alice, 1}, {tim, 1}],
      fun(Alice, Tim) ->
        %% Tim's previous temp subscription should have been cleared
        %% by his disconnection
        Stanza = expect_iq_success(subscribers_stanza(), Alice),
        check_subscribers(Stanza, [?ALICE_B_JID, ?CAROL_B_JID, ?KAREN_B_JID]),

        subscribe_temporary(?BOT_B_JID, Tim),
        timer:sleep(400),

        Stanza2 = expect_iq_success(subscribers_stanza(), Alice),
        check_subscribers(Stanza2, [?ALICE_B_JID, ?CAROL_B_JID, ?KAREN_B_JID,
                                   escalus_client:full_jid(Tim)
                                  ]),

        unsubscribe_temporary(?BOT_B_JID, Tim),

        Stanza3 = expect_iq_success(subscribers_stanza(), Alice),
        check_subscribers(Stanza3, [?ALICE_B_JID, ?CAROL_B_JID, ?KAREN_B_JID]),
        ensure_all_clean([Alice, Tim])
      end).

temporary_unsubscribe_roster_change(Config) ->
    escalus:story([everyone_is_friends | Config], [{alice, 1}, {tim, 1}],
      fun(Alice, Tim) ->
        subscribe_temporary(?BOT_B_JID, Tim),
        timer:sleep(400),

        Stanza = expect_iq_success(subscribers_stanza(), Alice),
        check_subscribers(Stanza, [?ALICE_B_JID, ?CAROL_B_JID, ?KAREN_B_JID,
                                   escalus_client:full_jid(Tim)
                                  ]),

        escalus:send(Alice, escalus_stanza:roster_remove_contact(Tim)),
        escalus:assert_many([is_presence, is_roster_set, is_iq_result],
                            escalus:wait_for_stanzas(Alice, 3)),

        %% Tim should get alerted of his loss of access to the bot
        escalus:assert_many([escalus_pred:is_presence_with_type(
                               <<"unsubscribe">>, _),
                             escalus_pred:is_presence_with_type(
                               <<"unavailable">>, _),
                             is_roster_set,
                             is_bot_unsubscribe(_)],
                            escalus:wait_for_stanzas(Tim, 4)),

        Stanza2 = expect_iq_success(subscribers_stanza(), Alice),
        check_subscribers(Stanza2, [?ALICE_B_JID, ?CAROL_B_JID, ?KAREN_B_JID]),

        timer:sleep(500),

        %% All done - set it back as it was
        set_visibility(Alice, ?WOCKY_BOT_VIS_WHITELIST, ?BOT)
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
    wocky_db:clear_tables(shared, [bot]),
    escalus:story(Config, [{alice, 1}, {bob, 1}, {tim, 1}],
      fun(Alice, Bob, Tim) ->
        IDs = [create_simple_bot(Alice) || _ <- lists:seq(1, ?CREATED_BOTS)],
        {_OwnerBots, FriendsOnlyBots, FollowersBots} = distribute(IDs),

        FriendsBots = lists:sort(wocky_util:v1_uuid_order(_, _),
                                 FriendsOnlyBots ++ FollowersBots),

        set_visibility(Alice, ?WOCKY_BOT_VIS_FRIENDS, FriendsOnlyBots),

        set_visibility(Alice, ?WOCKY_BOT_VIS_FOLLOWERS, FollowersBots),

        %% Alice can see all her bots
        Stanza = expect_iq_success(
                   retrieve_stanza(?ALICE_B_JID, #rsm_in{}), Alice),
        check_returned_bots(Stanza, IDs, 0, ?CREATED_BOTS),

        Stanza2 = expect_iq_success(
                   retrieve_stanza(?ALICE_B_JID,
                                   #rsm_in{direction = before}), Alice),
        check_returned_bots(Stanza2, IDs, 0, ?CREATED_BOTS),

        %% Bob can only see the subset of bots set to be visible by friends
        %% and followers
        Stanza3 = expect_iq_success(
                    retrieve_stanza(?ALICE_B_JID, #rsm_in{}), Bob),
        check_returned_bots(Stanza3, FriendsBots,
                            0, length(FriendsBots)),

        %% Tim cannot see any of Alice's bots since he is neither
        %% the owner nor a friend nor follower
        Stanza4 = expect_iq_success(
                    retrieve_stanza(?ALICE_B_JID, #rsm_in{}), Tim),
        check_returned_bots(Stanza4, [], undefined, 0),

        %% Make Tim a follower of Alice
        test_helper:subscribe(Tim, Alice),

        %% Tim can now see the follower bots:
        Stanza5 = expect_iq_success(
                    retrieve_stanza(?ALICE_B_JID, #rsm_in{}), Tim),
        check_returned_bots(Stanza5, FollowersBots, 0, length(FollowersBots)),

        %% Test some basic RSM functionality
        %% Bob can only see the subset of bots set to be visible by friends
        Stanza6 = expect_iq_success(
                    retrieve_stanza(?ALICE_B_JID,
                                    #rsm_in{index = 3, max = 2}), Bob),
        ExpectedBots = lists:sublist(FriendsBots, 4, 2),
        check_returned_bots(Stanza6, ExpectedBots, 3,
                            length(FriendsBots)),

        %% When alice publishes to a bot, that bot should become the most
        %% recently updated, moving it to the end of the list:
        PublishBot = lists:nth(10, IDs),
        NoteID = <<"Note">>,
        Title = <<"Title">>,
        Content = <<"Content">>,
        publish_item(PublishBot, NoteID, Title, Content, undefined, Alice),

        %% Alice can see all her bots with the updated one now at the end
        Stanza7 = expect_iq_success(
                   retrieve_stanza(?ALICE_B_JID, #rsm_in{}), Alice),
        check_returned_bots(Stanza7, (IDs -- [PublishBot]) ++ [PublishBot],
                            0, ?CREATED_BOTS)

      end).

get_subscribed(Config) ->
    reset_tables(Config),
    escalus:story(Config, [{alice, 1}, {bob, 1}, {karen, 1}],
      fun(Alice, Bob, Karen) ->
        %% Make the bot open, otherwise even followers won't be able
        %% to see it if they're not affiliates
        expect_iq_success(
          change_visibility_stanza(?BOT, ?WOCKY_BOT_VIS_OPEN), Alice),

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

update_affiliations(Config) ->
    reset_tables(Config),
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}],
      fun(Alice, Bob, Carol) ->
        NewAffiliations = [{?BOB_B_JID, none},
                           {?CAROL_B_JID, spectator}],

        test_helper:add_contact(Alice, Bob, <<"affiliates">>, <<"BMan">>),
        test_helper:add_contact(Alice, Carol, <<"affiliates">>, <<"Caz">>),

        % Let the bot's roster catch up
        timer:sleep(500),

        % Alice can modify affiliations on her contacts
        expect_iq_success(modify_affiliations_stanza(NewAffiliations), Alice),

        Stanza = expect_iq_success(affiliations_stanza(), Alice),
        check_affiliations(Stanza, [{?CAROL_B_JID, spectator},
                                    {?ALICE_B_JID, owner}]),

        % Carol and Bob get notified of their changed status
        expect_affiliation_update(Carol, spectator),
        expect_affiliation_update(Bob, none),

        %% Alice removes Bob as a contact
        escalus:send(Alice, escalus_stanza:roster_remove_contact(Bob)),
        escalus:assert_many([is_roster_set, is_iq_result],
                            escalus:wait_for_stanzas(Alice, 2)),
        escalus:assert(is_presence_with_type, [<<"unavailable">>],
                       escalus:wait_for_stanza(Bob)),

        timer:sleep(500),

        % Alice can't add a non-roster affiliate
        expect_iq_error(
          modify_affiliations_stanza([{?BOB_B_JID, spectator}]), Alice),

        % Bob can't because he's not the owner
        expect_iq_error(modify_affiliations_stanza(NewAffiliations), Bob)
      end).

friends_only_permissions(Config) ->
    reset_tables(Config),
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        expect_iq_success(
          change_visibility_stanza(?BOT, ?WOCKY_BOT_VIS_FRIENDS), Alice),
        %% Bob does not receive a visibility change because he's already an
        %% affiliate

        test_helper:add_contact(Alice, Bob, <<"friends">>, <<"Bobbie">>),

        %% Give the bot a moment to be notified and update its copy of
        %% Alice's roster
        timer:sleep(500),

        %% Bob is a member of Alice's roster, so should be able to get the bot
        expect_iq_success(retrieve_stanza(), Bob),

        %% Alice removes Bob as a contact
        escalus:send(Alice, escalus_stanza:roster_remove_contact(Bob)),
        escalus:assert_many([is_roster_set, is_iq_result],
                            escalus:wait_for_stanzas(Alice, 2)),

        % Bob was an affiliate at this point, so gets a notification that
        % he has been de-affiliated
        escalus:assert_many([is_pres_unavailable(),
                             is_affiliation_update(
                               escalus_client:short_jid(Bob), none, _)],
                            escalus:wait_for_stanzas(Bob, 2)),

        % Bob can't get the bot because he's no longer a contact
        expect_iq_error(retrieve_stanza(), Bob),

        %% Alice adds Bob back as a contact
        test_helper:add_contact(Alice, Bob, <<"friends">>, <<"Bobbie">>),
        test_helper:subscribe_pair(Alice, Bob),

        timer:sleep(500),

        %% Bob is a member of Alice's roster, so should be able to get the bot
        expect_iq_success(retrieve_stanza(), Bob),

        %% Set the bot back to WHITELIST
        expect_iq_success(
          change_visibility_stanza(?BOT, ?WOCKY_BOT_VIS_WHITELIST), Alice),

        %% Alice removes Bob as a contact so that subsequent tests don't fail
        escalus:send(Alice, escalus_stanza:roster_remove_contact(Bob)),
        escalus:assert_many([is_roster_set, is_iq_result,
                            escalus_pred:is_presence_with_type(
                              <<"unavailable">>, _)],
                            escalus:wait_for_stanzas(Alice, 3)),

        escalus:assert_many([is_roster_set,
                             escalus_pred:is_presence_with_type(
                               <<"unsubscribe">>, _)],
                       escalus:wait_for_stanzas(Bob, 2))
      end).

roster_change_triggers(Config) ->
    reset_tables(Config),
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1},
                           {karen, 1}, {robert, 1}, {tim, 1}],
      fun(Alice, Bob, Carol, Karen, Robert, Tim) ->
        % Robert is nothin', so gets no bot notifications
        escalus:send(Alice, escalus_stanza:roster_remove_contact(Robert)),
        escalus:assert_many([is_roster_set, is_iq_result],
                            escalus:wait_for_stanzas(Alice, 2)),

        escalus:assert(is_pres_unavailable(), escalus:wait_for_stanza(Robert)),

        % Bob is an affiliate but not subscriber
        escalus:send(Alice, escalus_stanza:roster_remove_contact(Bob)),
        escalus:assert_many([is_roster_set, is_iq_result],
                            escalus:wait_for_stanzas(Alice, 2)),

        escalus:assert_many([is_pres_unavailable(),
                             is_affiliation_update(
                               escalus_client:short_jid(Bob), none, _)],
                            escalus:wait_for_stanzas(Bob, 2)),

        % Carol is a subscriber but not affiliate
        escalus:send(Alice, escalus_stanza:roster_remove_contact(Carol)),
        escalus:assert_many([is_roster_set, is_iq_result],
                            escalus:wait_for_stanzas(Alice, 2)),

        escalus:assert_many([is_pres_unavailable(),
                             is_bot_unsubscribe(_)],
                            escalus:wait_for_stanzas(Carol, 2)),

        % Carol is both a subscriber and affiliate (after we affiliate and
        % subscribe her)
        expect_iq_success(modify_affiliations_stanza(
                            [{?KAREN_B_JID, spectator}]), Alice),
        escalus:assert(
          is_affiliation_update(escalus_client:short_jid(Karen), spectator, _),
          escalus:wait_for_stanza(Karen)),
        expect_iq_success(subscribe_stanza(), Karen),

        escalus:send(Alice, escalus_stanza:roster_remove_contact(Karen)),
        escalus:assert_many([is_roster_set, is_iq_result],
                            escalus:wait_for_stanzas(Alice, 2)),

        escalus:assert_many([is_pres_unavailable(),
                             is_affiliation_update(
                               escalus_client:short_jid(Karen), none, _),
                             is_bot_unsubscribe(_)],
                            escalus:wait_for_stanzas(Karen, 3)),

        %% Now, set the bot to followers visibility
        set_visibility(Alice, ?WOCKY_BOT_VIS_FOLLOWERS, [?BOT]),

        %% Make Tim a follower of Alice and of the bot
        test_helper:subscribe(Tim, Alice),
        expect_iq_success(subscribe_stanza(), Tim),

        %% Boot Tim off as a follower of Alice
        escalus:send(Alice, escalus_stanza:roster_remove_contact(Tim)),
        escalus:assert_many([is_roster_set, is_iq_result],
                            escalus:wait_for_stanzas(Alice, 2)),

        %% Tim should get alerted of his loss of access to the bot
        escalus:assert_many([escalus_pred:is_presence_with_type(
                               <<"unsubscribed">>, _),
                             is_roster_set,
                             is_bot_unsubscribe(_)],
                            escalus:wait_for_stanzas(Tim, 3)),

        timer:sleep(500),

        test_helper:ensure_all_clean([Alice, Bob, Carol, Karen, Robert, Tim])
      end).

blocked_group(Config) ->
    reset_tables(Config),
    escalus:story(Config, [{alice, 1}, {tim, 1}],
      fun(Alice, Tim) ->
        expect_iq_success(
          change_visibility_stanza(?BOT, ?WOCKY_BOT_VIS_FRIENDS), Alice),

        % Alice adds Tim as a normal friend
        test_helper:add_contact(Alice, Tim, <<"blah">>,
                                <<"He's okay">>),
        test_helper:subscribe_pair(Alice, Tim),
        timer:sleep(500),

        % Tim can view the bot
        expect_iq_success(retrieve_stanza(), Tim),

        % Alice moves Tim to the magic __blocked__ group
        test_helper:add_contact(Alice, Tim, <<"__blocked__">>,
                                <<"MyMortalEnemy">>),
        escalus:assert(is_presence_with_type, [<<"unavailable">>],
                       escalus:wait_for_stanza(Tim)),
        timer:sleep(500),

        % Tim can no longer view the bot
        expect_iq_error(retrieve_stanza(), Tim)
      end).

delete_owner(Config) ->
    reset_tables(Config),
    escalus:story(Config, [{alice, 1}, {bob, 1}, {tim, 1}],
      fun(Alice, Bob, Tim) ->
        ID1 = create_simple_bot(Alice),

        expect_iq_success(
          change_visibility_stanza(ID1, ?WOCKY_BOT_VIS_FRIENDS), Alice),

        ReturnedBot = expect_iq_success(retrieve_stanza(ID1), Bob),
        ExpectedFieldsFriends =
        lists:keyreplace("visibility", 1, expected_simple_bot_fields(),
                         {"visibility", int, ?WOCKY_BOT_VIS_FRIENDS}),
        check_returned_bot(ReturnedBot, ExpectedFieldsFriends),
        expect_iq_error(retrieve_stanza(ID1), Tim),

        %% Delete Alice
        expect_iq_success(
          user_SUITE:delete_request(), Alice),
        R = escalus:wait_for_stanza(Alice, 3000),
        escalus:assert(is_stream_error,
                       [<<"conflict">>, <<"User removed">>], R),
        timer:sleep(500),
        ?assertNot(escalus_connection:is_connected(Alice)),

        %% Access to the bot should be unchanged, in spite of Alice (and by
        %% extension her roster) having been deleted. The FRIENDS bot should,
        %% however, have been changed to WHITELIST visibility.
        WhitelistFields =
        lists:keyreplace("visibility", 1, expected_simple_bot_fields(),
                         {"visibility", int, ?WOCKY_BOT_VIS_WHITELIST}),
        WhitelistFields2 =
        lists:keyreplace("owner", 1, WhitelistFields,
                         {"owner", jid, <<>>}),
        ExpectedFields =
        lists:keyreplace("affiliates+size", 1, WhitelistFields2,
                         {"affiliates+size", int, 4}),
        ExpectedFields2 =
        lists:keyreplace("subscribers+size", 1, ExpectedFields,
                         {"subscribers+size", int, 0}),
        ReturnedBot3 = expect_iq_success(retrieve_stanza(ID1), Bob),
        check_returned_bot(ReturnedBot3, ExpectedFields2),
        expect_iq_error(retrieve_stanza(ID1), Tim)
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
        % so should just have her 3 seeded HS items
        Stanza = expect_iq_success_u(get_hs_stanza(), Alice, Alice),
        check_hs_result(Stanza, 3, 0, false),

        % Nobody else can publish an item to the bot besides the owner
        expect_iq_error(
          publish_item_stanza(?BOT, NoteID, Title, Content),
          Bob),
        expect_iq_error(
          publish_item_stanza(?BOT, NoteID, Title, Content),
          Carol),

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
    wocky_db:clear_tables(?LOCAL_CONTEXT, [bot_item]),
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}, {karen, 1}],
      fun(Alice, Bob, Carol, Karen) ->
        % Alice publishes a bunch of items on her bot
        lists:foreach(
          add_item(Alice, [Carol, Karen], _), lists:seq(0, ?CREATED_ITEMS-1)),

        % Bob can get items because he's an affiliate
        get_items(Bob, #rsm_in{max = 10}, 0, 9),
        get_items(Bob, #rsm_in{index = 5},
                  5, ?CREATED_ITEMS-1),
        get_items(Bob, #rsm_in{max = 2, direction = before, id = item_id(5)},
                  3, 4),
        get_items(Bob, #rsm_in{max = 3, direction = aft, id = item_id(48)},
                  49, min(?CREATED_ITEMS-1, 51)),
        get_items(Bob, #rsm_in{max = 3, direction = before},
                  ?CREATED_ITEMS-3, ?CREATED_ITEMS-1),

        % Carol can't because she's not
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
        lists:keyreplace("image_items", 1, expected_retrieve_fields(),
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
    wocky_db:truncate(?LOCAL_CONTEXT, home_stream),
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
        [] = wocky_db:select(shared, bot_share, all, #{bot => ?BOT_B_JID}),
        set_visibility(Alice, ?WOCKY_BOT_VIS_FRIENDS, [?BOT]),

        %% Alice can't share to Tim because he's not a friend
        escalus:send(Alice, share_stanza(?BOT, Alice, Tim)),
        escalus:assert(is_error, [<<"cancel">>, <<"not-allowed">>],
                       escalus:wait_for_stanza(Alice)),

        [] = wocky_db:select(shared, bot_share, all, #{bot => ?BOT_B_JID}),

        %% Make Tim a friend
        test_helper:add_contact(Alice, Tim, <<"blah">>, <<"He's okay">>),
        test_helper:subscribe_pair(Alice, Tim),

        %% Alice can now share to him
        escalus:send(Alice, share_stanza(?BOT, Alice, Tim)),
        timer:sleep(500),

        ExpectedTo = ?TIM_B_JID,
        ExpectedFrom = ?ALICE_B_JID,
        [#{to_jid   := ExpectedTo,
           from_jid := ExpectedFrom}] = wocky_db:select(shared, bot_share, all,
                                                        #{bot => ?BOT_B_JID}),

        test_helper:ensure_all_clean([Alice, Tim])
      end).

share_open(Config) ->
    reset_tables(Config),
    wocky_db:truncate(shared, roster),
    escalus:story(Config, [{alice, 1}, {carol, 1}, {tim, 1}],
      fun(Alice, Carol, Tim) ->
        set_visibility(Alice, ?WOCKY_BOT_VIS_OPEN, [?BOT]),

        % Tim can't see the bot because it hasn't been shared to him
        expect_iq_error(retrieve_stanza(), Tim),

        %% Make Tim a friend of Alice
        test_helper:add_contact(Alice, Tim, <<"blah">>, <<"He's okay">>),
        test_helper:subscribe_pair(Alice, Tim),

        %% Alice can now share to him
        escalus:send(Alice, share_stanza(?BOT, Alice, Tim)),
        timer:sleep(500),

        %% Tim can now see the bot
        expect_iq_success(retrieve_stanza(), Tim),

        %% Carol can't see the bot because it hasn't been shared to her
        expect_iq_error(retrieve_stanza(), Carol),

        %% Make Carol a friend of Tim
        test_helper:subscribe_pair(Carol, Tim),

        %% Tim shares the bot to Carol
        escalus:send(Tim, share_stanza(?BOT, Tim, Carol)),
        timer:sleep(500),

        %% Carol can now see the bot
        expect_iq_success(retrieve_stanza(), Carol),

        2 = length(wocky_db:select(shared, bot_share, all,
                                   #{bot => ?BOT_B_JID})),

        test_helper:ensure_all_clean([Alice, Carol, Tim])
      end).

geosearch(Config) ->
    reset_tables(Config),
    escalus:story(Config, [{alice, 1}],
      fun(Alice) ->
        Stanza = expect_iq_success(add_to_s(geosearch_stanza(), Alice), Alice),
        check_geosearch_return(Stanza)
      end).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

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
     {"type",          "string", ?CREATE_TYPE}
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
    create_field(Name, "geoloc", geoloc_element(Value)).

value_element(Value) ->
    #xmlel{name = <<"value">>, children = [#xmlcdata{content = Value}]}.

geoloc_element({Lat, Lon}) ->
    #xmlel{name = <<"geoloc">>,
           attrs = [{<<"xmlns">>, ?NS_GEOLOC}],
           children = [coordinate_element(<<"lat">>, Lat),
                       coordinate_element(<<"lon">>, Lon)]}.

coordinate_element(Name, Val) ->
    #xmlel{name = Name,
           children = [#xmlcdata{content = float_to_binary(Val)}]}.

create_field(Name, Type, Child) ->
    #xmlel{name = <<"field">>,
           attrs = [{<<"var">>, list_to_binary(Name)},
                    {<<"type">>, list_to_binary(Type)}],
           children = [Child]}.

expected_create_fields() ->
    [{"id",                 string, any},
     {"server",             string, ?LOCAL_CONTEXT},
     {"title",              string, ?CREATE_TITLE},
     {"shortname",          string, ?CREATE_SHORTNAME},
     {"owner",              jid,    ?ALICE_B_JID},
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
     {"affiliates+size",    int,    1}, % Owner is always an affiliate
     {"affiliates+hash",    string, any},
     {"subscribers+size",   int,    1}, % Owner is always a subscriber
     {"subscribers+hash",   string, any}].

expected_retrieve_fields() ->
    [{"id",                 string, ?BOT},
     {"server",             string, ?LOCAL_CONTEXT},
     {"title",              string, ?BOT_TITLE},
     {"shortname",          string, ?BOT_NAME},
     {"owner",              jid,    ?ALICE_B_JID},
     {"description",        string, ?BOT_DESC},
     {"address",            string, ?BOT_ADDRESS},
     {"image",              string, ?AVATAR_FILE},
     {"type",               string, ?BOT_TYPE},
     {"location",           geoloc, {?BOT_LAT, ?BOT_LON}},
     {"radius",             int,    ?BOT_RADIUS},
     {"visibility",         int,    ?WOCKY_BOT_VIS_WHITELIST},
     {"alerts",             int,    ?WOCKY_BOT_ALERT_DISABLED},
     {"jid",                jid,    bot_jid(?BOT)},
     {"image_items",        int,    1},
     {"updated",            timestamp, any},
     {"affiliates+size",    int,    2}, % Owner is always an affiliate
     {"affiliates+hash",    string, any},
     {"subscribers+size",   int,    3}, % Owner is always an subscriber
     {"subscribers+hash",   string, any}].

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
    #xmlel{name = <<"bot">>, attrs = [{<<"xmlns">>, ?NS_BOT}],
           children = Children} = BotStanza,
    check_return_fields(Children, ExpectedFields),
    % Return the bot ID
    get_id(Children).

check_returned_bots(#xmlel{name = <<"iq">>, children = [BotsStanza]},
                    ExpectedIDs, Index, Total) ->
    #xmlel{name = <<"bots">>, attrs = [{<<"xmlns">>, ?NS_BOT}],
           children = Children} = BotsStanza,
    {First, Last} = case ExpectedIDs of
                        [] -> {undefined, undefined};
                        _ -> {hd(ExpectedIDs), lists:last(ExpectedIDs)}
                    end,
    ?assertEqual(ok,
       do([error_m ||
           RSM <- check_get_children(Children, <<"set">>,
                                     [{<<"xmlns">>, ?NS_RSM}]),
           RSMOut <- decode_rsm(RSM),
           check_rsm(RSMOut, Total, Index, First, Last),
           check_ids(ExpectedIDs, Children)
          ])
      ).

check_geosearch_return(#xmlel{name = <<"iq">>, children = [BotsStanza]}) ->
    #xmlel{name = <<"bots">>, attrs = [{<<"xmlns">>, ?NS_BOT}],
           children = Children} = BotsStanza,
    lists:foreach(
      fun (#xmlel{name = <<"bot">>, children = Fields}) ->
              check_return_fields(Fields, expected_geosearch_fields())
      end,
      Children).

check_ids(ExpectedIDs, Children) ->
    IDs = get_ids(Children, []),
    case ExpectedIDs of
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

check_return_fields(Elements, ExpectedFields) ->
    lists:foreach(check_field(_, Elements), ExpectedFields).

check_field({Name, Type, any}, Elements) ->
    ?assert(has_field(list_to_binary(Name), atom_to_binary(Type, utf8),
            Elements));
check_field({Name, Type, Value}, Elements) ->
    ?assert(has_field_val(list_to_binary(Name), atom_to_binary(Type, utf8),
                          Value, Elements)).

has_field(Name, Type, Elements) ->
    find_field(Name, Type, Elements) =/= false.

has_field_val(Name, Type, Value, Elements) ->
    case find_field(Name, Type, Elements) of
        #xmlel{name = <<"field">>, children = [ValueEl]} ->
            check_value_el(Value, Type, ValueEl);
        X ->
            ct:fail("find_field ~p ~p ~p returned ~p",
                    [Name, Type, Elements, X])
    end.

check_value_el(Value, <<"geoloc">>, El = #xmlel{name = <<"geoloc">>}) ->
    check_geoloc_val(Value, El);
check_value_el(<<>>, Type,
               #xmlel{name = <<"value">>,
                      children = []})
    when Type =:= <<"string">> orelse Type =:= <<"jid">> ->
    true;
check_value_el(Value, Type,
               #xmlel{name = <<"value">>,
                      children = [#xmlcdata{content = Value}]})
    when Type =:= <<"string">> orelse Type =:= <<"jid">> ->
    true;
check_value_el(Value, <<"int">>,
               #xmlel{name = <<"value">>,
                      children = [#xmlcdata{content = InValue}]}) ->
    Value =:= binary_to_integer(InValue);
check_value_el(Value, Type, Element) ->
    ct:fail("check_value_el failed: ~p ~p ~p", [Value, Type, Element]).

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
    jid:to_binary(jid:make(<<>>, ?LOCAL_CONTEXT, bot_node(ID))).

change_visibility_stanza(Bot, Visibility) ->
    test_helper:iq_set(?NS_BOT, node_el(Bot, <<"fields">>,
                                        [visibility_field(Visibility)])).

visibility_field(Visibility) ->
    create_field({"visibility", "int", Visibility}).

update_stanza() ->
    test_helper:iq_set(?NS_BOT, node_el(?BOT, <<"fields">>, [modify_field()])).

modify_field() ->
    create_field({"description", "string", ?NEW_DESCRIPTION}).

affiliations_stanza() ->
    test_helper:iq_get(?NS_BOT, node_el(?BOT, <<"affiliations">>)).

check_affiliations(#xmlel{name = <<"iq">>, children = [AffiliationsEl]},
                   Affiliates) ->
    Affiliations = AffiliationsEl#xmlel.children,
    ?assertEqual({value, integer_to_binary(length(Affiliates))},
                 xml:get_attr(<<"size">>, AffiliationsEl#xmlel.attrs)),
    ?assertEqual(length(Affiliates), length(Affiliations)),
    lists:foreach(check_affiliation(Affiliations, _), Affiliates).

check_affiliation(AffiliationEls, Affiliate) ->
    case lists:any(is_affiliate(Affiliate, _), AffiliationEls) of
        true -> true;
        false -> ct:fail("Missing affiliate ~p", [Affiliate])
    end.

is_affiliate({Name, Type}, #xmlel{name = <<"affiliation">>, attrs = Attrs}) ->
    xml:get_attr(<<"jid">>, Attrs) =:= {value, Name} andalso
    xml:get_attr(<<"affiliation">>, Attrs) =:=
     {value, atom_to_binary(Type, utf8)}.

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

modify_affiliations_stanza(NewAffiliations) ->
    test_helper:iq_set(?NS_BOT,
                       node_el(?BOT, <<"affiliations">>,
                               affiliation_els(NewAffiliations))).

affiliation_els(Affiliations) ->
    [affiliation_el(JID, Role) || {JID, Role} <- Affiliations].

affiliation_el(JID, Role) ->
    #xmlel{name = <<"affiliation">>,
           attrs = [{<<"jid">>, JID},
                    {<<"affiliation">>, atom_to_binary(Role, utf8)}]}.

expect_affiliation_update(Client, Type) ->
    Stanza = escalus:wait_for_stanza(Client),
    escalus:assert(
      is_affiliation_update(escalus_client:short_jid(Client), Type, _), Stanza).

is_affiliation_update(Name, Type,
                      #xmlel{name = <<"message">>,
                                children = [AffiliationsEl]}) ->
    is_affiliation_element(Name, Type, AffiliationsEl);
is_affiliation_update(_, _, _) ->false.

is_affiliation_element(Name, Type,
                       #xmlel{name = <<"affiliations">>, attrs = Attrs,
                              children = [AffiliateEl]}) ->
    has_standard_attrs(Attrs) andalso
    is_affiliate({Name, Type}, AffiliateEl);
is_affiliation_element(_, _, _) ->false.

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
    {A, B, C} = distribute(L, [], [], []),
    {lists:reverse(A), lists:reverse(B), lists:reverse(C)}.

distribute([], A, B, C) ->
    {A, B, C};
distribute([H], A, B, C) ->
    {[H|A], B, C};
distribute([H,H2], A, B, C) ->
    {[H|A], [H2|B], C};
distribute([H,H2,H3|T], A, B, C) ->
    distribute(T, [H|A], [H2|B], [H3|C]).

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
    Remaining = check_image(?ALICE_B_JID, ?ITEM, ?ITEM_IMAGE, ImageList),
    RSMXML = lists:foldl(
               fun(I, S) ->
                       check_image(?ALICE_B_JID, item_id(I),
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
    Expiry = wocky_db:now_to_timestamp(os:timestamp()) div 1000 + ExpiryPeriod,
    QueryEl = #xmlel{name = <<"follow-me">>,
                     attrs = [
                       {<<"node">>, bot_node(?BOT)},
                       {<<"expiry">>, integer_to_binary(Expiry)}
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
      #xmlel{name = <<"message">>,
             attrs = [{<<"type">>, <<"headline">>}],
             children = [cdata_el(<<"body">>, <<"Here's a bot!">>),
                         bot_el(BotID)]},
      escalus_stanza:to(Target),
      escalus_stanza:from(From)
     ).

bot_el(BotID) ->
    #xmlel{name = <<"bot">>,
           attrs = [{<<"xmlns">>, ?NS_BOT}],
           children = [cdata_el(<<"jid">>,
                                jid:to_binary(
                                  wocky_bot_util:make_jid(
                                    ?LOCAL_CONTEXT, BotID))),
                       cdata_el(<<"id">>, BotID),
                       cdata_el(<<"server">>, ?LOCAL_CONTEXT),
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
