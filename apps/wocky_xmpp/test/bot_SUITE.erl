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
                      check_hs_result/3, expect_iq_success_u/3,
                      publish_item_stanza/4, publish_item_stanza/5,
                      retract_item_stanza/2, subscribe_stanza/0,
                      node_el/2, node_el/3, cdata_el/2,
                      ensure_all_clean/1, query_el/1,
                      add_to_s/2, set_notifications/2,
                      check_home_stream_sizes/2,
                      check_home_stream_sizes/3,
                      watch_hs/1, unwatch_hs/1
                     ]).

-export([create_bot_stanza/1, set_visibility/3,
         old_create_field/1, publish_item/6,
         delete_stanza/1]).

-define(BOT_TITLE, <<"Alice's Bot">>).
-define(BOT_NAME, <<"AliceBot">>).
-define(BOT_DESC, <<"A test bot owned by Alice">>).
-define(BOT_ADDRESS, <<"260 Tinakori Road, Thorndon, Wellington">>).
-define(BOT_ADDRESS_DATA, <<"{name: foo}">>).
-define(BOT_TYPE, <<"LucyLiuBot">>).
-define(BOT_LAT, 55.0).
-define(BOT_LON, 60.1).
-define(BOT_RADIUS, 10000.0).

-define(CREATE_TITLE,       <<"Created Bot">>).
-define(CREATE_SHORTNAME,   <<"NewBot">>).
-define(CREATE_DESCRIPTION, <<"Test bot for creation operation">>).
-define(CREATE_ADDRESS,     <<"5 Adelaide Avenue, Deakin, ACT">>).
-define(CREATE_ADDRESS_DATA,<<"{name: bar}">>).
-define(CREATE_LOCATION,    {2.5, 1.6}).
-define(CREATE_RADIUS,      10.0).
-define(CREATE_IMAGE,
        <<"tros:localhost/file/231d325e-bde7-11e7-aeea-e3fd897704af">>).
-define(CREATE_TYPE,        <<"floatbot">>).
-define(CREATE_TAGS,        [<<"tag1">>, <<"tag2">>]).

-define(NEW_DESCRIPTION,    <<"New bot description!">>).

-define(ALICE_HANDLE, <<"AliceHandle">>).
-define(AVATAR_ID, <<"cc25d9de-8b8b-11e7-ad2c-83478732a270">>).
-define(AVATAR_URL, ?tros:make_url(?AVATAR_ID)).
-define(ALICE_FIRST_NAME, <<"Alice">>).
-define(ALICE_LAST_NAME, <<"Alison">>).

-define(CREATED_BOTS,       30).
-define(CREATED_ITEMS,      50).

-define(BOBS_ITEM_ID, <<"item2">>).

-define(wocky_push_sandbox, 'Elixir.Wocky.Push.Sandbox').

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, old_interface},
     {group, new_interface},
     retrieve,
     subscribers_no_rsm,
     subscribers_rsm,
     unsubscribe,
     subscribe,
     subscribe_geofence,
     watch,
     unwatch,
     delete,
     errors,
     retrieve_for_user,
     get_subscribed,
     sorting,
     publish_item,
     retract_item,
     edit_item,
     get_items,
     publish_image_item,
     item_images,
     share,
     share_multicast,
     geofence_share,
     open_visibility,
     geosearch,
     empty_shortname,
     explore_nearby_radius,
     explore_nearby_rectangle,
     get_guests,
     get_visitors
    ].

groups() ->
    [{old_interface, [], multi_interface()},
     {new_interface, [], multi_interface()}].

%Groups
multi_interface() ->
    [
     create,
     new_id,
     update,
     errors,
     empty_shortname
    ].

suite() ->
    escalus:suite().


%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),
    NewConfig = reset_tables(Config),
    [{interface, new} | NewConfig].

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(old_interface, Config) -> set_interface(old, Config);
init_per_group(new_interface, Config) -> set_interface(new, Config).

end_per_group(_, _) -> ok.

set_interface(I, Config) ->
    reset_tables(Config),
    [{interface, I} | proplists:delete(interface, Config)].

init_per_testcase(geosearch, Config) ->
    meck:new('Elixir.Wocky.Index', [passthrough]),
    meck:expect(
        'Elixir.Wocky.Index', geosearch,
        fun (_Lat, _Lon) ->
                {ok, [#{id => ?BOT, server => ?SERVER,
                        user_id => ?ALICE, title => ?BOT_TITLE,
                        image => ?CREATE_IMAGE,
                        location => ?wocky_geo_utils:point(?BOT_LAT, ?BOT_LON),
                        radius => ?BOT_RADIUS,
                        distance => 8000}]}
        end),
    escalus:init_per_testcase(geosearch, Config);
init_per_testcase(EN, Config) when EN =:= explore_nearby_radius
                              orelse EN =:= explore_nearby_rectangle ->
    ?wocky_repo:delete_all(?wocky_bot),
    Alice = ?wocky_repo:get(?wocky_user, ?ALICE),
    Bots =
    lists:map(
      fun(I) ->
              ?wocky_factory:insert(bot, #{user => Alice,
                                           location =>
                                           ?wocky_geo_utils:point(I, I)})
      end,
      lists:seq(1, 10)),
    escalus:init_per_testcase(geosearch, [{bots, Bots} | Config]);
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

    ?wocky_factory:insert(tros_metadata, #{id => ?AVATAR_ID,
                                           user => Alice,
                                           access => <<"all">>}),

    {ok, _} = ?wocky_user:update(?ALICE, #{handle => ?ALICE_HANDLE,
                                           avatar => ?AVATAR_URL,
                                           first_name => ?ALICE_FIRST_NAME,
                                           last_name => ?ALICE_LAST_NAME}),

    B = #{id => ?BOT,
          title => ?BOT_TITLE,
          shortname => ?BOT_NAME,
          user => Alice,
          description => ?BOT_DESC,
          location => ?wocky_geo_utils:point(?BOT_LAT, ?BOT_LON),
          radius => ?BOT_RADIUS,
          address => ?BOT_ADDRESS,
          address_data => ?BOT_ADDRESS_DATA,
          image => ?AVATAR_FILE,
          type => ?BOT_TYPE},
    Bot = ?wocky_factory:insert(bot, B),

    ?wocky_factory:insert(tros_metadata, [{id, ?ITEM_IMAGE_ID},
                                          {user, Alice},
                                          {access, <<"all">>}]),
    ?wocky_item:put(Bot, Alice, ?ITEM, ?ITEM_STANZA),
    ?wocky_item:put(Bot, Alice, ?ITEM2, ?ITEM_STANZA2),

    ?wocky_share:put(Bob, Bot, Alice),
    ?wocky_bot:subscribe(Bot, Carol),
    ?wocky_bot:subscribe(Bot, Karen),

    [{bot, Bot}, {carol, Carol} | Config2].

%%--------------------------------------------------------------------
%% mod_wocky_bot tests
%%--------------------------------------------------------------------

create(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        % Successfully create a bot
        Stanza = expect_iq_success(create_stanza(Config), Alice),
        check_returned_bot(Stanza, expected_create_fields()),

        % No home stream updates should occur for private bots
        timer:sleep(400),
        check_home_stream_sizes(0, [Alice, Bob]),

        CreateFields = [{"public", "bool", true} |
                        lists:keydelete("shortname", 1, default_fields())],
        expect_iq_success(create_stanza(CreateFields, Config), Alice),

        % Both the creator and their friends and followers should get
        % HS notifications for public bots:
        timer:sleep(400),
        check_home_stream_sizes(1, [Alice, Bob], false),

        % Check functionality of deprecated integer radius
        CreateFields2 = [{"radius", "int", 10} |
                         lists:keydelete("radius", 1,
                                         lists:keydelete("shortname", 1,
                                                         default_fields()))],
        expect_iq_success(create_stanza(CreateFields2, Config), Alice),

        % Fail due to shortname conflict if we try to create the same bot
        expect_iq_error(create_stanza(Config), Alice)
      end).

new_id(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        %% Get a new ID
        Result = expect_iq_success(new_id_stanza(), Alice),
        ID = xml:get_path_s(Result, [{elem, <<"new-id">>}, cdata]),

        %% We can't specify an un-allocated ID for creation
        CreateFields = [{"id", "string", ?wocky_id:new()} |
                        lists:keydelete("shortname", 1, default_fields())],
        expect_iq_error(create_stanza(CreateFields, Config), Bob),
        expect_iq_error(create_stanza(CreateFields, Config), Alice),

        %% Alice can't yet get the bot because it hasn't been created
        expect_iq_error(retrieve_stanza(ID), Alice),

        %% Alice can publish to the bot ID
        publish_item(ID, <<"ID">>,
                     <<"title">>, <<"content">>, undefined, Alice),

        %% Bob can't since he's not the owner
        expect_iq_error(
          publish_item_stanza(ID, <<"ID">>, <<"title">>, <<"content">>), Bob),

        % Now create the bot
        CreateFields2 = [{"id", "string", ID} |
                         lists:keydelete("shortname", 1, default_fields())],
        expect_iq_success(create_stanza(CreateFields2, Config), Alice),

        %% Alice can now get the bot
        expect_iq_success(retrieve_stanza(ID), Alice)
      end).

retrieve(Config) ->
    reset_tables(Config),
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}],
      fun(Alice, Bob, Carol) ->
        % Alice can retrieve her own bot
        Stanza = expect_iq_success(retrieve_stanza(), Alice),
        check_returned_bot(Stanza, expected_retrieve_fields(true)),

        % Bob can retrieve the bot since it's shared to him
        Stanza2 = expect_iq_success(retrieve_stanza(), Bob),
        check_returned_bot(Stanza2, expected_retrieve_fields(false)),

        % Carol cannot retrive since the bot is not public
        expect_iq_error(retrieve_stanza(), Carol),

        % Invalid IDs should fail but not crash
        expect_iq_error(
          retrieve_stanza(<<"6ca1153a-dad4-11e7-83cf-0a580a02012c1">>),
          Alice)
      end).

update(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        % Alice can update the bot
        expect_iq_success(update_stanza(Config), Alice),

        % And the new bot should have the change
        Stanza = expect_iq_success(retrieve_stanza(), Alice),
        NewFields =
            lists:keyreplace("description", 1, expected_retrieve_fields(true),
                            {"description", string, ?NEW_DESCRIPTION}),
        check_returned_bot(Stanza, NewFields),

        % Bob can't update it since he's not the owner
        expect_iq_error(update_stanza(Config), Bob)
      end).

subscribers_no_rsm(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        % Alice can get the correct subscribers
        Stanza = expect_iq_success(subscribers_stanza(), Alice),
        check_subscribers(Stanza, [?BJID(?CAROL),
                                   ?BJID(?KAREN)]),

        % Bob can't because he's not the owner
        expect_iq_error(subscribers_stanza(), Bob)
      end).

subscribers_rsm(Config) ->
    escalus:story(Config, [{alice, 1}],
      fun(Alice) ->
        % Alice can get the correct subscribers
        Stanza = expect_iq_success(subscribers_stanza(
                                     #rsm_in{max = 1}), Alice),
        check_subscribers(Stanza, [?BJID(?CAROL)], 2)
      end).


unsubscribe(Config) ->
    escalus:story(Config, [{alice, 1}, {carol, 1}],
      fun(Alice, Carol) ->
        % Alice cannot unsubscribe herself
        expect_iq_error(unsubscribe_stanza(), Alice),

        % But Carol can
        Stanza1 = expect_iq_success(unsubscribe_stanza(), Carol),
        check_subscriber_count(Stanza1, 1),

        % Alice can get the correct subscribers
        Stanza2 = expect_iq_success(subscribers_stanza(), Alice),
        check_subscribers(Stanza2, [?BJID(?KAREN)])
      end).

subscribe(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}],
      fun(Alice, Bob, Carol) ->
        set_visibility(Alice, ?WOCKY_BOT_VIS_OPEN, ?BOT),

        check_returned_bot(expect_iq_success(retrieve_stanza(), Carol),
                           expected_retrieve_fields(false, ?BOT_DESC,
                                                    ?WOCKY_BOT_VIS_OPEN, 1)),
        Stanza1 = expect_iq_success(subscribe_stanza(), Carol),
        check_returned_bot(expect_iq_success(retrieve_stanza(), Carol),
                           expected_retrieve_fields(true, ?BOT_DESC,
                                                    ?WOCKY_BOT_VIS_OPEN, 2)),
        check_subscriber_count(Stanza1, 2),

        Stanza2 = expect_iq_success(subscribe_stanza(), Bob),
        check_subscriber_count(Stanza2, 3),

        % Alice can get the correct subscribers
        Stanza3 = expect_iq_success(subscribers_stanza(), Alice),
        check_subscribers(Stanza3, [?BJID(?KAREN),
                                    ?BJID(?CAROL),
                                    ?BJID(?BOB)])
      end).

subscribe_geofence(Config) ->
    reset_tables(Config),
    escalus:story(Config, [{alice, 1}, {carol, 1}],
      fun(Alice, Carol) ->
          set_visibility(Alice, ?WOCKY_BOT_VIS_OPEN, ?BOT),

          % There are no guests...
          check_returned_bot(expect_iq_success(retrieve_stanza(), Carol),
                             expected_guest_retrieve_fields(false, 0)),

          set_notifications(true, Alice),

          % Carol becomes a guest...
          expect_iq_success(subscribe_guest_stanza(true), Carol),
          check_returned_bot(expect_iq_success(retrieve_stanza(), Carol),
                             expected_guest_retrieve_fields(true, 1)),

          timer:sleep(500),
          1 = length(list_notifications()),
          clear_notifications(),

          % Carol cancels guesthood...
          expect_iq_success(subscribe_guest_stanza(false), Carol),
          check_returned_bot(expect_iq_success(retrieve_stanza(), Carol),
                             expected_guest_retrieve_fields(false, 0))
      end).

watch(Config) ->
    reset_tables(Config),
    escalus:story(Config, [{alice, 1}, {tim, 1}],
      fun(Alice, Tim) ->
        set_visibility(Alice, ?WOCKY_BOT_VIS_OPEN, ?BOT),

        check_returned_bot(expect_iq_success(retrieve_stanza(), Tim),
                           expected_retrieve_fields(false, ?BOT_DESC,
                                                    ?WOCKY_BOT_VIS_OPEN, 2)),
        watch(?BOT_B_JID, Tim),
        timer:sleep(500),

        % Should not affect subscriptions
        check_returned_bot(expect_iq_success(retrieve_stanza(), Tim),
                           expected_retrieve_fields(false, ?BOT_DESC,
                                                    ?WOCKY_BOT_VIS_OPEN, 2)),

        expect_iq_success(update_stanza(<<"TestDesc">>, Config), Alice),
        S = escalus_client:wait_for_stanza(Tim),
        ?assert(is_bot_update_notification(S)),

        ensure_all_clean([Alice, Tim])
      end).

unwatch(Config) ->
    escalus:story(Config, [{alice, 1}, {tim, 1}],
      fun(Alice, Tim) ->
        set_visibility(Alice, ?WOCKY_BOT_VIS_OPEN, ?BOT),

        %% Tim's previous temp subscription should have been cleared
        %% by his disconnection
        Stanza = expect_iq_success(subscribers_stanza(), Alice),
        check_subscribers(Stanza, [?BJID(?CAROL), ?BJID(?KAREN)]),

        watch(?BOT_B_JID, Tim),
        unwatch(?BOT_B_JID, Tim),

        expect_iq_success(update_stanza(<<"BrandShinyNewDesc">>, Config),
                          Alice),
        timer:sleep(500),

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
        expect_iq_error(create_stanza(Missing, Config), Alice),

        Extra = [{"unknownfield", "string", <<"abc">>} | default_fields()],
        expect_iq_error(create_stanza(Extra, Config), Alice),

        NewConfig = [{interface, old} | proplists:delete(interface, Config)],
        WrongType = [{"title", "int", 10} | Missing],
        expect_iq_error(create_stanza(WrongType, NewConfig), Alice)
      end).

retrieve_for_user(Config) ->
    reset_tables(Config),
    ?wocky_repo:delete_all(?wocky_bot),
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        IDs = [create_simple_bot(Alice, Config)
               || _ <- lists:seq(1, ?CREATED_BOTS)],
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
           {"address_data",string, ?CREATE_ADDRESS_DATA},
           {"location",    geoloc, ?CREATE_LOCATION},
           {"radius",      float,  ?CREATE_RADIUS},
           {"image",       string, ?CREATE_IMAGE},
           {"type",        string, ?CREATE_TYPE},
           {"owner",       jid,    ?BJID(?ALICE)},
           {"visibility",  int,    ?WOCKY_BOT_VIS_OPEN},
           {"jid",         jid,    bot_jid(PublishBot)},
           {"subscribed",  bool,   false}]),

        Stanza9 = expect_iq_success(retrieve_stanza(?BJID(?ALICE),
                                                    sort_elem(0.0, 0.0),
                                                    #rsm_in{}),
                                    Alice),
        check_returned_bots(Stanza9, IDs, 0, ?CREATED_BOTS)
      end).

get_subscribed(Config) ->
    reset_tables(Config),
    escalus:story(Config, [{alice, 1}, {bob, 1}, {karen, 1}],
      fun(Alice, Bob, Karen) ->
        set_visibility(Alice, ?WOCKY_BOT_VIS_OPEN, ?BOT),

        %% Alice is the owner and so is automatically subscribed
        Stanza = expect_iq_success(subscribed_stanza(#rsm_in{}), Alice),
        check_returned_bots(Stanza, [?BOT], 0, 1),

        %% Karen is a subscriber so should get the bot
        Stanza2 = expect_iq_success(subscribed_stanza(#rsm_in{}), Karen),
        check_returned_bots(Stanza2, [?BOT], 0, 1),

        %% Bob is not subscribed to the bot at all so gets nothing
        Stanza3 = expect_iq_success(subscribed_stanza(#rsm_in{}), Bob),
        check_returned_bots(Stanza3, [], undefined, 0),

        Stanza4 = expect_iq_success(subscribed_stanza(0.0, 0.0, #rsm_in{}),
                                    Karen),
        check_returned_bots(Stanza4, [?BOT], 0, 1)
      end).

sorting(Config) ->
    AliceUser = ?wocky_repo:get(?wocky_user, ?ALICE),
    ?wocky_repo:delete_all(?wocky_home_stream_item),
    ?wocky_repo:delete_all(?wocky_bot),
    Bots = ?wocky_factory:insert_list(10, bot,
                                      #{user => AliceUser, shortname => nil}),
    escalus:story(Config, [{alice, 1}],
      fun(Alice) ->
        % Simple ascending order on title
        Stanza = expect_iq_success(
                   retrieve_stanza(?BJID(?ALICE),
                                   sort_elem(<<"asc">>, <<"title">>),
                                   #rsm_in{}),
                   Alice),

        IDsByTitle = sort_bot_ids(Bots, title),
        check_returned_bots(Stanza, IDsByTitle, 0, 10),

        % Reverse sort order on title, subset by index
        Stanza2 = expect_iq_success(
                    retrieve_stanza(?BJID(?ALICE),
                                    sort_elem(<<"desc">>, <<"title">>),
                                    #rsm_in{index = 5, max = 4}),
                    Alice),

        ReverseTitleSubset = lists:sublist(lists:reverse(IDsByTitle), 6, 4),
        check_returned_bots(Stanza2, ReverseTitleSubset, 5, 10),

        % Ascending by creation time, subset by ID
        IDsByCreated = sort_bot_ids(Bots, created_at),
        Stanza3 = expect_iq_success(
                    retrieve_stanza(?BJID(?ALICE),
                                    sort_elem(<<"asc">>, <<"created">>),
                                    #rsm_in{id = lists:nth(3, IDsByCreated),
                                            direction = aft}),
                    Alice),

        AscCreationSubset = lists:sublist(IDsByCreated, 4, 10),
        check_returned_bots(Stanza3, AscCreationSubset, 3, 10),

        % Descending by update time - first bot should be the just-updated one
        UpdatedBot = lists:nth(5, Bots),
        ?wocky_bot:bump_update_time(UpdatedBot),
        Stanza4 = expect_iq_success(
                    retrieve_stanza(?BJID(?ALICE),
                                    sort_elem(<<"desc">>, <<"updated">>),
                                    #rsm_in{index = 0, max = 1}),
                    Alice),
        check_returned_bots(Stanza4, [maps:get(id, UpdatedBot)], 0, 10),

        % Invalid sorting options
        expect_iq_error(
          retrieve_stanza(?BJID(?ALICE),
                          sort_elem(<<"desk">>, <<"updated">>),
                          #rsm_in{}),
          Alice),
        expect_iq_error(
          retrieve_stanza(?BJID(?ALICE),
                          sort_elem(<<"desc">>, <<"turtle">>),
                          #rsm_in{}),
          Alice),
        expect_iq_error(
          retrieve_stanza(?BJID(?ALICE),
                          bad_distance_sort_elem(1.0, 1.0),
                          #rsm_in{}),
          Alice),
        expect_iq_success(
          retrieve_stanza(?BJID(?ALICE),
                          sort_elem(1.0, 1.0),
                          #rsm_in{}),
          Alice)
      end).

publish_item(Config) ->
    reset_tables(Config),
    escalus:story(Config,
                  [{alice, 1}, {bob, 1}, {carol, 1},
                   {karen, 1}, {robert, 1}],
      fun(Alice, Bob, Carol, Karen, Robert) ->
        NoteID = <<"item1">>,
        Content = <<"content ZZZ">>,
        Title = <<"title ZZZ">>,

        watch(?BOT_B_JID, Bob),

        % Alice publishes an item to her bot
        publish_item(?BOT, NoteID, Title, Content, undefined, Alice),

        expect_item_pub_notification(Bob),
        % Carol and Karen are subscribers, and so receive a notification
        % Bob is a viewer (via share) but not subscribed,
        % so does not receive anything
        lists:foreach(
          expect_item_publication(_, ?BOT, NoteID, Title, Content),
          [Carol, Karen]),

        % As the publisher, Alice should *not* get a notification,
        % so her HS should be empty
        Stanza = expect_iq_success_u(get_hs_stanza(), Alice, Alice),
        check_hs_result(Stanza, 0, false),

        % As someone to whom the bot has been shared, Bob can publish items
        % to the bot and all the subscribers are notified.
        publish_item_watching(
          ?BOT, ?BOBS_ITEM_ID, Title, Content, undefined, Bob),
        lists:foreach(
          expect_item_publication(_, ?BOT, ?BOBS_ITEM_ID, Title, Content),
          [Alice, Carol, Karen]),

        % Bob can update his own item and subscribers will be informed.
        NewContent = <<"New Content">>,
        publish_item_watching(
          ?BOT, ?BOBS_ITEM_ID, Title, NewContent, undefined, Bob),
        lists:foreach(
          expect_item_publication(_, ?BOT, ?BOBS_ITEM_ID, Title, NewContent),
          [Alice, Carol, Karen]),

        % Robert can't view the bot so can't publish a note to it:
        expect_iq_error(
          publish_item_stanza(?BOT, <<"robertsnote">>, Title, Content),
          Robert),

        % Nobody else can publish to an item except the item owner
        expect_iq_error(
          publish_item_stanza(?BOT, NoteID, Title, Content),
          Bob),
        expect_iq_error(
          publish_item_stanza(?BOT, ?BOBS_ITEM_ID, Title, Content),
          Alice),

        % Alice cannot publish to a non-existant bot
        expect_iq_error(
          publish_item_stanza(?wocky_id:new(), NoteID, Title, Content), Alice),

        unwatch(?BOT, Bob),

        test_helper:ensure_all_clean([Alice, Bob, Carol, Karen])
      end).

retract_item(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}, {karen, 1}],
      fun(Alice, Bob, Carol, Karen) ->
        % Nobody else can publish an item to the bot besides the item or bot
        % owners
        expect_iq_error(
          retract_item_stanza(?BOT, ?ITEM),
          Bob),
        expect_iq_error(
          retract_item_stanza(?BOT, ?ITEM),
          Carol),

        % Alice can retract the item as its owner
        retract_item(?BOT, ?ITEM, Alice),

        % Bob can retract his own item
        retract_item(?BOT, ?BOBS_ITEM_ID, Bob),

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
               test_helper:iq_get(
                 ?NS_BOT, item_query_el(#rsm_in{max = 10})), Carol),

        test_helper:ensure_all_clean([Alice, Bob, Carol, Karen])
      end).

publish_image_item(Config) ->
    reset_tables(Config),
    %% Wait for DB notifications to fire
    timer:sleep(400),
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
        Expected2 =
        lists:keyreplace("total_items", 1, Expected,
                         {"total_items", int, 3}),
        Stanza = expect_iq_success(retrieve_stanza(), Alice),
        check_returned_bot(Stanza, Expected2),

        test_helper:ensure_all_clean([Alice])
      end).

item_images(Config) ->
    reset_tables(Config),
    escalus:story(Config, [{alice, 1}, {bob, 1}, {tim, 1}],
      fun(Alice, Bob, Tim) ->
        ItemIDs = lists:seq(0, 9),
        AliceUser = ?wocky_repo:get(?wocky_user, ?ALICE),
        Images =
        lists:map(
          fun(_) ->
                  ?wocky_factory:insert(tros_metadata, [{user, AliceUser},
                                                        {access, <<"all">>}])
          end, ItemIDs),
        ImageIDs = lists:map(maps:get(id, _), Images),
        IDPairs = lists:zip(ItemIDs, ImageIDs),
        lists:map(publish_item_with_image(_, Alice), IDPairs),

        %% Alice can see all the images
        Stanza = expect_iq_success(item_image_stanza(), Alice),
        check_returned_images(Stanza, IDPairs),

        %% So can bob who can access the bot
        Stanza2 = expect_iq_success(item_image_stanza(), Bob),
        check_returned_images(Stanza2, IDPairs),

        %% Tim cannot since he can't see the bot
        expect_iq_error(item_image_stanza(), Tim)
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
        escalus:send(Alice, share_stanza(?BOT, Alice, Tim, false)),
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
                   escalus_stanza:to(share_stanza(?BOT, false), ?SERVER)
                  ),
        escalus:send(Alice, Stanza),
        timer:sleep(500),

        % Tim can now see the bot
        expect_iq_success(retrieve_stanza(), Tim),

        [N] = list_notifications(),
        ?assert(
           binary:match(extract_alert_body(N), <<"shared a bot">>) =/= nomatch),

        test_helper:ensure_all_clean([Alice, Tim])
      end).

geofence_share(Config) ->
    reset_tables(Config),
    clear_notifications(),
    escalus:story(Config, [{alice, 1}, {tim, 1}],
      fun(Alice, Tim) ->
        set_visibility(Alice, ?WOCKY_BOT_VIS_FRIENDS, [?BOT]),
        timer:sleep(400),

        set_notifications(true, Tim),
        watch_hs(Tim),

        escalus:send(Alice, share_stanza(?BOT, Alice, Tim, true)),

        escalus:assert(is_message, escalus:wait_for_stanza(Tim)),

        timer:sleep(400),

        [N] = list_notifications(),
        ?assert(
           binary:match(extract_alert_body(N),
                        <<"wants to know">>) =/= nomatch),

        unwatch_hs(Tim),

        % Tim can now see the bot
        expect_iq_success(retrieve_stanza(), Tim),

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
        expect_iq_success(create_stanza(Fields, Config), Alice)
      end).

explore_nearby_radius(Config) ->
    escalus:story(Config, [{alice, 1}],
      fun(Alice) ->
        % Complete result set
        expect_iq_success(add_to_s(explore_nearby_stanza(100000000.0, 100),
                                   Alice), Alice),
        lists:foreach(
          expect_explore_result(_, Alice),
          proplists:get_value(bots, Config)
         ),
        expect_explore_result(<<"no-more-bots">>, Alice),

        % Tiny radius
        expect_iq_success(add_to_s(explore_nearby_stanza(160000.0, 100),
                                   Alice), Alice),
        expect_explore_result(hd(proplists:get_value(bots, Config)), Alice),
        expect_explore_result(<<"no-more-bots">>, Alice),

        % Limit bot count
        expect_iq_success(add_to_s(explore_nearby_stanza(100000000.0, 1),
                                   Alice), Alice),
        expect_explore_result(hd(proplists:get_value(bots, Config)), Alice),
        expect_explore_result(<<"bot-limit-reached">>, Alice)
      end).

explore_nearby_rectangle(Config) ->
    escalus:story(Config, [{alice, 1}],
      fun(Alice) ->
        % Complete result set
        expect_iq_success(add_to_s(explore_nearby_stanza(25.0, 25.0, 100),
                                   Alice), Alice),
        lists:foreach(
          expect_explore_result(_, Alice),
          proplists:get_value(bots, Config)
         ),
        expect_explore_result(<<"no-more-bots">>, Alice),

        % Tiny radius
        expect_iq_success(add_to_s(explore_nearby_stanza(2.5, 2.5, 100),
                                   Alice), Alice),
        expect_explore_result(hd(proplists:get_value(bots, Config)), Alice),
        expect_explore_result(<<"no-more-bots">>, Alice),

        % Limit bot count
        expect_iq_success(add_to_s(explore_nearby_stanza(25.0, 25.0, 1),
                                   Alice), Alice),
        expect_explore_result(hd(proplists:get_value(bots, Config)), Alice),
        expect_explore_result(<<"bot-limit-reached">>, Alice)
      end).

get_guests(Config) ->
    Config2 = reset_tables(Config),
    Bot = proplists:get_value(bot, Config2),
    CarolU = proplists:get_value(carol, Config2),
    ?wocky_bot:update(Bot, #{geofence => true}),
    ?wocky_bot:subscribe(Bot, CarolU, true),
    escalus:story(Config2, [{alice, 1}, {bob, 1}, {carol, 1}],
      fun(Alice, Bob, Carol) ->
        S = expect_iq_success(get_users_stanza(<<"guests">>, ?BOT), Alice),
        check_users_result(S, <<"guests">>, <<"guest">>, [?ALICE, ?CAROL]),

        % Non-owners can't get the guest list
        expect_iq_error(get_users_stanza(<<"guests">>, ?BOT), Bob),
        expect_iq_error(get_users_stanza(<<"guests">>, ?BOT), Carol)
      end).

get_visitors(Config) ->
    Config2 = reset_tables(Config),
    Bot = proplists:get_value(bot, Config2),
    CarolU = proplists:get_value(carol, Config2),
    ?wocky_bot:update(Bot, #{geofence => true}),
    ?wocky_bot:subscribe(Bot, CarolU, true),
    ?wocky_bot:visit(Bot, CarolU),
    escalus:story(Config2, [{alice, 1}, {bob, 1}, {carol, 1}],
      fun(Alice, Bob, Carol) ->
        Results = [expect_iq_success(
                     get_users_stanza(<<"visitors">>, ?BOT), C)
                   || C <- [Alice, Carol]],
        [check_users_result(R, <<"visitors">>, <<"visitor">>, [?CAROL])
         || R <- Results],

        % Non-guests can't get the visitor list
        expect_iq_error(get_users_stanza(<<"visitors">>, ?BOT), Bob)
      end).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

clear_notifications() ->
    ?wocky_push_sandbox:clear_notifications([{global, true}]).

list_notifications() ->
    ?wocky_push_sandbox:wait_notifications([{count, 1}, {global, true}]).

new_id_stanza() ->
    test_helper:iq_set(?NS_BOT, #xmlel{name = <<"new-id">>}).

create_bot_stanza(Fields) ->
    create_stanza(Fields, [{interface, new}]).

create_stanza(Config) ->
    create_stanza(default_fields(), Config).
create_stanza(Fields, Config) ->
    Interface = proplists:get_value(interface, Config),
    test_helper:iq_set(?NS_BOT,
                       #xmlel{name = <<"create">>,
                              children = create_fields(Fields, Interface)}).

default_fields() ->
    [{"title",         "string", ?CREATE_TITLE},
     {"shortname",     "string", ?CREATE_SHORTNAME},
     {"description",   "string", ?CREATE_DESCRIPTION},
     {"address",       "string", ?CREATE_ADDRESS},
     {"address_data",  "string", ?CREATE_ADDRESS_DATA},
     {"location",      "geoloc", ?CREATE_LOCATION},
     {"radius",        "float",  ?CREATE_RADIUS},
     {"image",         "string", ?CREATE_IMAGE},
     {"type",          "string", ?CREATE_TYPE},
     {"tags",          "tags",   ?CREATE_TAGS}
    ].

create_fields(Fields, old) ->
    [old_create_field(F) || F <- Fields];
create_fields(Fields, new) ->
    [create_field(F) || F <- Fields].

old_create_field({Name, "string", Value}) ->
    old_create_field(Name, "string", value_element(Value));
old_create_field({Name, "int", Value}) ->
    old_create_field(Name, "int", value_element(integer_to_binary(Value)));
old_create_field({Name, "float", Value}) ->
    old_create_field(Name, "float", value_element(float_to_binary(Value)));
old_create_field({Name, "jid", Value}) ->
    old_create_field(Name, "jid", value_element(jid:to_binary(Value)));
old_create_field({Name, "geoloc", Value}) ->
    old_create_field(Name, "geoloc", geoloc_element(Value));
old_create_field({Name, "tags", Values}) ->
    old_create_field(Name, "tags",
                 [wocky_xml:cdata_el(<<"tag">>, V) || V <- Values]);
old_create_field({Name, "bool", Value}) ->
    old_create_field(Name, "bool", value_element(atom_to_binary(Value, utf8))).

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

old_create_field(Name, Type, Children) when is_list(Children) ->
    #xmlel{name = <<"field">>,
           attrs = [{<<"var">>, list_to_binary(Name)},
                    {<<"type">>, list_to_binary(Type)}],
           children = Children};
old_create_field(Name, Type, Child) ->
    old_create_field(Name, Type, [Child]).

create_field({Name, "string", Value}) ->
    create_field(Name, Value);
create_field({Name, "int", Value}) ->
    create_field(Name, integer_to_binary(Value));
create_field({Name, "float", Value}) ->
    create_field(Name, float_to_binary(Value));
create_field({Name, "jid", Value}) ->
    create_field(Name, jid:to_binary(Value));
create_field({Name, "geoloc", Value}) ->
    create_field(Name, geoloc_element(Value));
create_field({Name, "tags", Values}) ->
    create_field(Name, [wocky_xml:cdata_el(<<"tag">>, V) || V <- Values]);
create_field({Name, "bool", Value}) ->
    create_field(Name, atom_to_binary(Value, utf8)).

create_field(Name, Value) when is_binary(Value) ->
    #xmlel{name = list_to_binary(Name),
           children = [#xmlcdata{content = Value}]};
create_field(Name, Children) when is_list(Children) ->
    #xmlel{name = list_to_binary(Name),
           children = Children};
create_field(Name, Child = #xmlel{}) ->
    create_field(Name, [Child]).

expected_create_fields() ->
    [{"id",                 string, any},
     {"server",             string, ?SERVER},
     {"title",              string, ?CREATE_TITLE},
     {"shortname",          string, ?CREATE_SHORTNAME},
     {"owner",              jid,    ?BJID(?ALICE)},
     {"description",        string, ?CREATE_DESCRIPTION},
     {"address",            string, ?CREATE_ADDRESS},
     {"address_data",       string, ?CREATE_ADDRESS_DATA},
     {"image",              string, ?CREATE_IMAGE},
     {"type",               string, ?CREATE_TYPE},
     {"location",           geoloc, ?CREATE_LOCATION},
     {"radius",             float,  ?CREATE_RADIUS},
     {"visibility",         int,    ?WOCKY_BOT_VIS_OWNER},
     {"jid",                jid,    any},
     {"image_items",        int,    0},
     {"total_items",        int,    0},
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
     {"address_data",       string, ?BOT_ADDRESS_DATA},
     {"image",              string, ?AVATAR_FILE},
     {"type",               string, ?BOT_TYPE},
     {"location",           geoloc, {?BOT_LAT, ?BOT_LON}},
     {"radius",             float,  ?BOT_RADIUS},
     {"visibility",         int,    Visibility},
     {"jid",                jid,    bot_jid(?BOT)},
     {"image_items",        int,    1},
     {"total_items",        int,    2},
     {"updated",            timestamp, any},
     {"subscribed",         bool,   Subscribed},
     {"subscribers+size",   int,    Subscribers},
     {"subscribers+hash",   string, any}
    ].

expected_guest_retrieve_fields(Guest, Guests) ->
    [{"guest",       bool,   Guest},
     {"guests+size", int,    Guests},
     {"guests+hash", string, any}
     | expected_retrieve_fields(true, ?BOT_DESC, ?WOCKY_BOT_VIS_OPEN, any)
    ].

expected_geosearch_fields() ->
    [{"jid",                jid,    any},
     {"owner",              jid,    any},
     {"id",                 string, any},
     {"server",             string, any},
     {"title",              string, any},
     {"image",              string, any},
     {"location",           geoloc, any},
     {"radius",             float,  any},
     {"distance",           float,  any}].

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
    end;
get_id([_ | Rest]) -> get_id(Rest).

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
check_value_el(Value, <<"float">>,
               [#xmlel{name = <<"value">>,
                       children = [#xmlcdata{content = InValue}]}]) ->
    Value =:= binary_to_float(InValue);
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

retrieve_stanza() ->
    retrieve_stanza(?BOT).

retrieve_stanza(BotID) when is_binary(BotID)->
    test_helper:iq_get(?NS_BOT, node_el(BotID, <<"bot">>)).

retrieve_stanza(User, RSM) ->
    test_helper:iq_get(?NS_BOT,
                       #xmlel{name = <<"bot">>,
                              attrs = [{<<"user">>, User}],
                              children = [rsm_elem(RSM)]}).

retrieve_stanza(User, SortElem, RSM) ->
    test_helper:iq_get(?NS_BOT,
                       #xmlel{name = <<"bot">>,
                              children = [owner_elem(User),
                                          SortElem,
                                          rsm_elem(RSM)]}).

subscribed_stanza(Sort1, Sort2, RSM) ->
    test_helper:iq_get(?NS_BOT, #xmlel{name = <<"subscribed">>,
                                       children = [sort_elem(Sort1, Sort2),
                                                   rsm_elem(RSM)]}).

subscribed_stanza(RSM) ->
    test_helper:iq_get(?NS_BOT, #xmlel{name = <<"subscribed">>,
                                       children = [rsm_elem(RSM)]}).

owner_elem(User) ->
    #xmlel{name = <<"owner">>, attrs = [{<<"jid">>, User}]}.

sort_elem(Lat, Lon) when is_float(Lat), is_float(Lon) ->
    #xmlel{name = <<"sort">>,
           attrs = [{<<"direction">>, <<"asc">>}, {<<"by">>, <<"distance">>}],
           children = [#xmlel{name = <<"near">>,
                              attrs = [{<<"lat">>, float_to_binary(Lat)},
                                       {<<"lon">>, float_to_binary(Lon)}]}]};

sort_elem(Dir, Field) ->
    #xmlel{name = <<"sort">>,
           attrs = [{<<"direction">>, Dir}, {<<"by">>, Field}]}.

bot_jid(ID) ->
    jid:to_binary(jid:make(<<>>, ?SERVER, bot_node(ID))).

change_visibility_stanza(Bot, Visibility) ->
    test_helper:iq_set(?NS_BOT, node_el(Bot, <<"fields">>,
                                        [visibility_field(Visibility)])).

bad_distance_sort_elem(Lat, Lon) ->
    #xmlel{name = <<"sort">>,
           attrs = [{<<"direction">>, <<"desc">>}, {<<"by">>, <<"distance">>}],
           children = [#xmlel{name = <<"near">>,
                              attrs = [{<<"lat">>, float_to_binary(Lat)},
                                       {<<"lon">>, float_to_binary(Lon)}]}]}.

visibility_field(Visibility) ->
    old_create_field({"visibility", "int", Visibility}).

update_stanza(Config) ->
    update_stanza(?NEW_DESCRIPTION, Config).
update_stanza(NewDesc, Config) when is_list(Config) ->
    Interface = proplists:get_value(interface, Config),
    update_stanza(NewDesc, Interface);
update_stanza(NewDesc, old) ->
    test_helper:iq_set(
      ?NS_BOT, node_el(?BOT, <<"fields">>, [modify_field(NewDesc)]));
update_stanza(NewDesc, new) ->
    test_helper:iq_set(
      ?NS_BOT, node_el(?BOT, <<"update">>,
                       [#xmlel{name = <<"description">>,
                               children = [#xmlcdata{content = NewDesc}]}])).

modify_field(NewDesc) ->
    old_create_field({"description", "string", NewDesc}).

subscribers_stanza() ->
    test_helper:iq_get(?NS_BOT, node_el(?BOT, <<"subscribers">>)).

subscribers_stanza(RSM) ->
    test_helper:iq_get(
      ?NS_BOT, node_el(?BOT, <<"subscribers">>, [rsm_elem(RSM)])).

check_subscribers(Stanza, Subscribers) ->
    check_subscribers(Stanza, Subscribers, length(Subscribers)).
check_subscribers(#xmlel{name = <<"iq">>, children = [SubscribersEl]},
                   Subscribers, Count) ->
    ReceivedSubscribers = SubscribersEl#xmlel.children,
    ?assertEqual({value, integer_to_binary(Count)},
                 xml:get_attr(<<"size">>, SubscribersEl#xmlel.attrs)),
    % Subtract 1 from ReceivedSubscribers for RSM element:
    ?assertEqual(length(Subscribers), length(ReceivedSubscribers) - 1),
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

check_guest_subscriber_count(Stanza = #xmlel{name = <<"iq">>}, ExpectedCount) ->
    Count = xml:get_path_s(Stanza, [{elem, <<"guests+size">>}, cdata]),
    ?assertEqual(binary_to_integer(Count), ExpectedCount).

is_bot_unsubscribe(#xmlel{name = <<"message">>, children = [Unsubscribed]}) ->
    Attrs = Unsubscribed#xmlel.attrs,
    <<"unsubscribed">> =:= Unsubscribed#xmlel.name andalso
    has_standard_attrs(Attrs);
is_bot_unsubscribe(_) -> false.

has_standard_attrs(Attrs) ->
    {value, bot_node(?BOT)} =:= xml:get_attr(<<"node">>, Attrs)  andalso
    {value, ?NS_BOT} =:= xml:get_attr(<<"xmlns">>, Attrs).

subscribe_guest_stanza(Switch) ->
    SubEl = node_el(?BOT, <<"subscribe">>, [make_geofence_el(Switch)]),
    test_helper:iq_set(?NS_BOT, SubEl).

make_geofence_el(Switch) ->
    cdata_el(<<"geofence">>, atom_to_binary(Switch, utf8)).

unsubscribe_stanza() ->
    test_helper:iq_set(?NS_BOT, node_el(?BOT, <<"unsubscribe">>)).

delete_stanza() -> delete_stanza(?BOT).
delete_stanza(Bot) ->
    test_helper:iq_set(?NS_BOT, node_el(Bot, <<"delete">>)).

expect_item_publication(Client, BotID, NoteID, Title, Content) ->
    S = expect_iq_success_u(get_hs_stanza(), Client, Client),
    I = check_hs_result(S, any, false),
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


expect_item_pub_notification(Client) ->
    ?assert(is_item_pub_notification(escalus_client:wait_for_stanza(Client))).

is_item_pub_notification(Stanza = #xmlel{name = <<"message">>}) ->
    R = xml:get_path_s(Stanza,
                       [{elem, <<"notification">>},
                        {elem, <<"item">>},
                        {elem, <<"message">>},
                        {elem, <<"event">>},
                        {elem, <<"item">>}]),
    case R of
        false -> false;
        _ -> true
    end;
is_item_pub_notification(_) -> false.

expect_item_retraction(Client, BotID, NoteID) ->
    S = expect_iq_success_u(get_hs_stanza(), Client, Client),
    I = check_hs_result(S, any, false),
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
               test_helper:iq_get(?NS_BOT, item_query_el(RSM)), Client),

    ?assertEqual(ok, check_result(Result, First, Last)).

item_query_el(RSM) -> item_query_el(RSM, ?BOT).
item_query_el(RSM, BotID) ->
    #xmlel{name = <<"query">>,
           attrs = [{<<"node">>, bot_node(BotID)}],
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
    ID = item_id(I),
    AliceBJID = ?BJID(?ALICE),

    lists:all(
      fun({K, V}) -> {value, V} =:= xml:get_attr(K, Attrs);
         (X)      -> X
      end,
      [
       {<<"id">>, ID},
       {<<"author">>, AliceBJID},
       {<<"author_avatar">>, ?AVATAR_URL},
       {<<"author_handle">>, ?ALICE_HANDLE},
       {<<"author_first_name">>, ?ALICE_FIRST_NAME},
       {<<"author_last_name">>, ?ALICE_LAST_NAME},
       is_item_entry(I, Entry)
      ]);
is_item(_, _) -> false.

is_item_entry(I, El = #xmlel{name = <<"entry">>,
                             attrs = [{<<"xmlns">>, ?NS_ATOM}]}) ->
    item_title(I) =:= xml:get_path_s(El, [{elem, <<"title">>}, cdata])
    andalso
    item_content(I) =:= xml:get_path_s(El, [{elem, <<"content">>}, cdata]);
is_item_entry(_, _) -> false.

item_id(undefined) -> undefined;
item_id(ItemID) when is_binary(ItemID) -> ItemID;
item_id(ItemID) ->
    <<"ID_", (integer_to_binary(ItemID))/binary>>.
item_title(ItemID) ->
    <<"Title_", (integer_to_binary(ItemID))/binary>>.
item_content(ItemID) ->
    <<"Content_", (integer_to_binary(ItemID))/binary>>.
item_image_url(ImageID) ->
    <<"tros:", ?ALICE/binary, "@", ?SERVER/binary, "/file/", ImageID/binary>>.

create_simple_bot(Client, Config) ->
    Fields = lists:keydelete("shortname", 1, default_fields()),
    Stanza = expect_iq_success(
               create_stanza(Fields, Config), Client),
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

publish_item_watching(BotID, NoteID, Title, Content, Image, Client) ->
    Stanza = add_to_s(
               publish_item_stanza(BotID, NoteID, Title, Content, Image),
               Client),
    escalus_client:send(Client, Stanza),
    Results = escalus_client:wait_for_stanzas(Client, 2),
    escalus:assert_many([is_iq_result, fun is_item_pub_notification/1],
                        Results).

publish_item(BotID, NoteID, Title, Content, Image, Client) ->
    expect_iq_success(publish_item_stanza(BotID, NoteID, Title, Content, Image),
                      Client).

retract_item(BotID, NoteID, Client) ->
    expect_iq_success(retract_item_stanza(BotID, NoteID), Client).

publish_item_with_image({ItemID, ImageID}, Client) ->
    publish_item(?BOT, item_id(ItemID), <<"Title">>, <<"Content">>,
                 item_image_url(ImageID), Client).

check_returned_images(#xmlel{name = <<"iq">>, children = Children}, IDPairs) ->
    [#xmlel{name = <<"item_images">>, children = ImageList}] = Children,
    Remaining = check_image(?BJID(?ALICE), ?ITEM, ?ITEM_IMAGE_ID, ImageList),
    RSMXML = lists:foldl(
               fun({ItemID, ImageID}, S) ->
                       check_image(?BJID(?ALICE), item_id(ItemID), ImageID, S)
               end, Remaining, IDPairs),
    {ok, RSMEls} = check_get_children(hd(RSMXML), <<"set">>,
                                      [{<<"xmlns">>, ?NS_RSM}]),
    {ok, RSM} = decode_rsm(RSMEls),
    ItemCount = length(IDPairs),
    ok = check_rsm(RSM, ItemCount + 1, 0, ?ITEM, item_id(ItemCount-1)).

check_image(Owner, Item, ImageID,
            [#xmlel{name = <<"image">>, attrs = Attrs} | Rest]) ->
    URL = item_image_url(ImageID),
    ?assertEqual({value, Owner}, xml:get_attr(<<"owner">>, Attrs)),
    ?assertEqual({value, Item}, xml:get_attr(<<"item">>, Attrs)),
    ?assertEqual({value, URL}, xml:get_attr(<<"url">>, Attrs)),
    ?assertEqual({value, <<"https://", ?SERVER/binary, "/", ImageID/binary,
                           "-thumbnail">>},
                 xml:get_attr(<<"thumbnail_url">>, Attrs)),
    ?assertEqual({value, <<"https://", ?SERVER/binary, "/", ImageID/binary>>},
                 xml:get_attr(<<"full_url">>, Attrs)),
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

watch(BotBJID, Client) ->
    Stanza = escalus_stanza:presence_direct(
               BotBJID, <<"available">>, [query_el(undefined)]),
    escalus:send(Client, Stanza).

unwatch(Bot, Client) ->
    Stanza = escalus_stanza:presence_direct(Bot, <<"unavailable">>),
    escalus:send(Client, Stanza).

share_stanza(BotID, From, Target, Geofence) ->
    fun_chain:first(
      BotID,
      share_stanza(Geofence),
      escalus_stanza:to(Target),
      escalus_stanza:from(From)
     ).

share_stanza(BotID, Geofence) ->
    #xmlel{name = <<"message">>,
           attrs = [{<<"type">>, <<"headline">>}],
           children = [cdata_el(<<"body">>, <<"Here's a bot!">>),
                       bot_el(BotID, Geofence)]}.

bot_el(BotID, Geofence) ->
    #xmlel{name = <<"bot">>,
           attrs = [{<<"xmlns">>, ?NS_BOT}],
           children = [cdata_el(<<"jid">>, bot_jid(BotID)),
                       cdata_el(<<"id">>, BotID),
                       cdata_el(<<"server">>, ?SERVER),
                       action_el(Geofence)]}.

action_el(false) -> cdata_el(<<"action">>, <<"share">>);
action_el(true) -> cdata_el(<<"action">>, <<"geofence share">>).

is_bot_action_hs_notification(JID, Action,
                              Stanza = #xmlel{name = <<"message">>}) ->
    case xml:get_path_s(Stanza,
                        [{elem, <<"notification">>},
                         {elem, <<"item">>}, {elem, <<"message">>}]) of
        <<>> -> false;
        SubStanza -> test_helper:is_bot_action(JID, Action, SubStanza)
    end;
is_bot_action_hs_notification(_, _, _) -> false.

is_bot_update_notification(Stanza = #xmlel{name = <<"message">>}) ->
    case xml:get_path_s(Stanza,
                        [{elem, <<"notification">>},
                         {elem, <<"item">>},
                         {elem, <<"message">>},
                         {elem, <<"bot-description-changed">>},
                         {elem, <<"bot">>}]) of
        <<>> -> false;
        _ -> true
    end;
is_bot_update_notification(_) -> false.

geosearch_stanza() ->
    QueryEl = #xmlel{name = <<"bots">>,
                     attrs = [
                       {<<"lat">>, float_to_binary(?BOT_LAT)},
                       {<<"lon">>, float_to_binary(?BOT_LON)}
                     ]},
    test_helper:iq_get(?NS_BOT, QueryEl).

explore_nearby_stanza(Radius, Limit) ->
    QueryEl =
    #xmlel{name = <<"bots">>,
           children = [#xmlel{name = <<"explore-nearby">>,
                              attrs = [{<<"limit">>, integer_to_binary(Limit)},
                                       {<<"radius">>,
                                        float_to_binary(Radius)},
                                       {<<"lat">>, float_to_binary(0.0)},
                                       {<<"lon">>, float_to_binary(0.0)}]}]},
    test_helper:iq_get(?NS_BOT, QueryEl).

explore_nearby_stanza(DeltaLat, DeltaLon, Limit) ->
    QueryEl =
    #xmlel{name = <<"bots">>,
           children = [#xmlel{name = <<"explore-nearby">>,
                              attrs = [{<<"limit">>, integer_to_binary(Limit)},
                                       {<<"lat_delta">>,
                                        float_to_binary(DeltaLat)},
                                       {<<"lon_delta">>,
                                        float_to_binary(DeltaLon)},
                                       {<<"lat">>, float_to_binary(0.0)},
                                       {<<"lon">>, float_to_binary(0.0)}]}]},
    test_helper:iq_get(?NS_BOT, QueryEl).

sort_bot_ids(Bots, Field) when Field =:= created_at;
                               Field =:= updated_at ->
    Sorted = lists:sort(fun(A, B) ->
                                ?datetime:compare(
                                   maps:get(Field, A), maps:get(Field, B)
                                  ) =/= gt
                        end, Bots),
    lists:map(maps:get(id, _), Sorted);
sort_bot_ids(Bots, Field) ->
    Sorted = lists:sort(fun(A, B) ->
                                maps:get(Field, A) =< maps:get(Field, B)
                        end, Bots),
    lists:map(maps:get(id, _), Sorted).

expect_explore_result(#{id := BotID}, User) ->
    Stanza = escalus_client:wait_for_stanza(User),
    ct:log("Explore result: ~p", [Stanza]),
    escalus:assert(is_message, Stanza),
    escalus:assert(has_type, [<<"headline">>], Stanza),
    IDEl = wocky_xml:path_by_attr(Stanza,
                                  [{element, <<"explore-nearby-result">>},
                                   {element, <<"bot">>},
                                   {element, <<"field">>}],
                                  <<"var">>, <<"id">>),
    {ok, Value} = wocky_xml:get_subel_cdata(<<"value">>, IDEl),
    ?assertEqual(Value, BotID);

expect_explore_result(Terminator, User) ->
    Stanza = escalus_client:wait_for_stanza(User),
    escalus:assert(is_message, Stanza),
    escalus:assert(has_type, [<<"headline">>], Stanza),
    ?assertNotEqual(
       exml_query:path(Stanza,
                       [{element, <<"explore-nearby-result">>},
                        {element, Terminator}]),
       undefined).

get_users_stanza(Name, BotID) ->
    test_helper:iq_get(
      ?NS_BOT,
      #xmlel{name = Name,
             attrs = [{<<"node">>, bot_node(BotID)}],
             children = [rsm_elem(#rsm_in{})]}).

check_users_result(Stanza, Name, ItemName, Users) ->
    ?assertEqual(
        exml_query:path(Stanza, [{element, Name},
                                {attr, <<"size">>}]),
        integer_to_binary(length(Users))),
    ?assertNotEqual(
        exml_query:path(Stanza, [{element, Name},
                                {attr, <<"hash">>}]),
        undefined),
    Items = (exml_query:path(Stanza, [{element, Name}]))#xmlel.children,
    check_users_items(ItemName, Items, Users).

check_users_items(_, [], []) -> ok;
check_users_items(ItemName, [#xmlel{name = <<"set">>} | Rest], Users) ->
    check_users_items(ItemName, Rest, Users);
check_users_items(ItemName, Items, [U|RestUsers]) ->
    RestItems = lists:filter(
                  fun(I) ->
                      not (
                          I#xmlel.name =:= ItemName andalso
                          exml_query:path(I, [{attr, <<"jid">>}]) =:=
                          jid:to_binary(jid:make(U, ?SERVER, <<>>)))
                  end, Items),
    ?assert(length(RestItems) =:= length(Items) - 1),
    check_users_items(ItemName, RestItems, RestUsers).

extract_alert_body(#{payload := #{<<"aps">> := #{<<"alert">> := Body}}}) ->
    Body;
extract_alert_body(_) ->
    ct:fail(alert_not_found).
