%%% @copyright 2016+ Hippware, Inc.
%%% @doc Test suite for wocky_db_bot.erl
-module(wocky_db_bot_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include("wocky_db_seed.hrl").
-include("wocky_bot.hrl").

-import(wocky_db_bot,
        [get_bot/1, get_bot/2, get_id_by_name/2, exists/2, insert/2,
         insert_new_name/2, owner/1,
         owned_bots/1, subscribed_bots/1, subscribers/2,
         subscribers/2, delete/2, has_access/3, subscribe/3, unsubscribe/3,
         get_item/3, publish_item/5, delete_item/3,
         subscribe_temporary/4, unsubscribe_temporary/3,
         clear_temporary_subscriptions/1,
         image_items_count/2, item_images/2,
         add_share/3, is_shared_to/2
        ]).

wocky_db_bot_test_() -> {
  "wocky_db_bot",
  setup, fun before_all/0, fun after_all/1,
  [ {inorder, [
      {inparallel, [
        test_get(),
        test_get_id_by_name(),
        test_exists(),
        test_insert(),
        test_insert_new_name(),
        test_owner(),
        test_owned(),
        test_subscribed(),
        test_subscribers(),
        test_image_items_count(),
        test_item_images(),
        test_share()
      ]},

      {inorder, [
        test_get_item(),
        test_publish_item(),
        test_delete_item()
      ]},

      {inorder, [
        test_subscribe_temporary(),
        test_unsubscribe_temporary(),
        test_clear_temporary_subscriptions()
      ]},

      {inorder, [
        test_has_access(),
        test_subscribe(),
        test_unsubscribe(),
        test_delete()
      ]}
    ]}
  ]}.

local_tables() -> [
                   bot_name,
                   bot_item
                  ].

before_all() ->
    ok = wocky_db_seed:seed_tables(shared, [roster, bot, bot_subscriber,
                                            bot_share]),
    ok = wocky_db_seed:seed_tables(?LOCAL_CONTEXT, local_tables()).

after_all(_) ->
    ok.

test_get() ->
    { "get_bot", [
      { "returns bot data if it exists", [
        ?_assertEqual(maps:without(
                        [updated],
                        hd(wocky_db_seed:seed_data(bot, ?LOCAL_CONTEXT))),
                      maps:without(
                        [affiliates, updated],
                        get_bot(?LOCAL_CONTEXT, ?BOT))),
        ?_assertEqual(maps:without(
                        [updated],
                        hd(wocky_db_seed:seed_data(bot, ?LOCAL_CONTEXT))),
                      maps:without(
                        [affiliates, updated],
                        get_bot(?BOT_JID)))
      ]},
      { "returns not_found if no bot exists", [
        ?_assertEqual(not_found, get_bot(?LOCAL_CONTEXT, wocky_db:create_id())),
        ?_assertEqual(not_found,
                      get_bot(jid:make(
                                <<>>, ?LOCAL_CONTEXT,
                                <<"bot/", (wocky_db:create_id())/binary>>)))
      ]}
    ]}.

test_get_id_by_name() ->
    { "get_id_by_name", [
      { "returns bot id if it exists", [
        ?_assertEqual(?BOT, get_id_by_name(?LOCAL_CONTEXT, ?BOT_NAME))
      ]},
      { "returns not_found if no bot exists with the supplied name", [
        ?_assertEqual(not_found, get_id_by_name(?LOCAL_CONTEXT,
                                                <<"other name">>))
      ]}
    ]}.

test_exists() ->
    { "exists", [
      { "returns true if a bot ID exists", [
        ?_assert(exists(?LOCAL_CONTEXT, ?BOT))
      ]},
      { "returns false if a bot does not exist", [
        ?_assertNot(exists(?LOCAL_CONTEXT, wocky_db:create_id()))
      ]}
    ]}.

test_insert() ->
    NewBot = #{id := ID} = new_bot(),
    { "insert", [
      { "inserts a new bot with the supplied parameters", inorder, [
        ?_assertEqual(ok, insert(?LOCAL_CONTEXT, NewBot)),
        ?_assertEqual(NewBot, maps:without([affiliates, updated],
                                           get_bot(?LOCAL_CONTEXT, ID)))
      ]}
    ]}.

test_insert_new_name() ->
    ID = wocky_db:create_id(),
    { "insert_new_name", [
      { "inserts a new name if that name is not already taken", inorder, [
        ?_assertEqual(ok, insert_new_name(ID, <<"brandnewname">>)),
        ?_assertEqual(ID, get_id_by_name(?LOCAL_CONTEXT, <<"brandnewname">>))
      ]},
      { "refuses to insert a new name if that name is already taken", inorder, [
        ?_assertEqual({error, exists},
                      insert_new_name(?BOT, ?BOT_NAME)),
        ?_assertEqual(?BOT, get_id_by_name(?LOCAL_CONTEXT, ?BOT_NAME))
      ]}
    ]}.

test_owner() ->
    { "owner", [
      { "gets the owner of the bot", [
        ?_assert(jid:are_bare_equal(?ALICE_JID, owner(?BOT)))
      ]},
      { "returns not_found if the bot doesn't exist", [
        ?_assertEqual(not_found, owner(wocky_db:create_id()))
      ]}
    ]}.

test_owned() ->
    { "owned_bots", [
      { "gets the list of bots owned by the user", [
        ?_assertEqual([?BOT_JID], owned_bots(?ALICE_JID))
      ]},
      { "returns an empty list if the user owns no bots", [
        ?_assertEqual([], owned_bots(?TIM_JID))
      ]}
    ]}.

test_subscribed() ->
    { "subscribed_bots", [
      { "gets the subscribed and owned bots for a user", inorder, [
        ?_assertEqual([?BOT_JID], subscribed_bots(?ALICE_JID)),
        ?_assertEqual([], subscribed_bots(?TIM_JID)),
        ?_assertEqual(ok, subscribe(?LOCAL_CONTEXT, ?BOT, ?TIM_JID)),
        ?_assertEqual([?BOT_JID], subscribed_bots(?TIM_JID))
      ]}
    ]}.

test_subscribers() ->
    { "subscribers", [
      { "returns the list of subscribers", [
        ?_assertEqual(lists:sort(base_subscribers()),
                      lists:sort(subscribers(?LOCAL_CONTEXT, ?BOT)))
      ]},
      { "returns empty list for non existant bot", [
        ?_assertEqual([], subscribers(?LOCAL_CONTEXT, wocky_db:create_id()))
      ]}
    ]}.

test_image_items_count() ->
    { "image_items_count", [
      { "returns the count of items on a bot with a <image> tag", [
        ?_assertEqual(1, image_items_count(?LOCAL_CONTEXT, ?BOT))
      ]},
      { "returns 0 for non existant bot", [
        ?_assertEqual(0, image_items_count(?LOCAL_CONTEXT,
                                           wocky_db:create_id()))
      ]}
    ]}.

test_item_images() ->
    { "item_images", [
      { "retrieves all images in items on a bot", [
        ?_assertMatch([#{id := ?ITEM,
                         updated := ?ITEM_UPDATE_TIME,
                         image := ?ITEM_IMAGE}],
                      item_images(?LOCAL_CONTEXT, ?BOT))
      ]},
      { "retrieves an emtpy list for a nonexistant bot", [
        ?_assertEqual([], item_images(?LOCAL_CONTEXT, wocky_db:create_id()))
      ]}
    ]}.

test_share() ->
    ID = <<"new_ID">>,
    JID = wocky_bot_util:make_jid(?LOCAL_CONTEXT, ID),
    { "add_share / is_shared_to", inorder, [
      { "Should not be marked as shared to other user until it's been shared", [
        ?_assertNot(is_shared_to(?BOB_JID, JID)),
        ?_assertEqual(ok, add_share(?ALICE_JID, ?BOB_JID, JID)),
        ?_assert(is_shared_to(?BOB_JID, JID))
      ]}
    ]}.

test_get_item() ->
    { "get_item", [
      { "gets all fields on the item", [
        ?_assertEqual(expected_item(), get_item(?LOCAL_CONTEXT, ?BOT, ?ITEM))
      ]},
      { "gets item_found on non-existant items", [
        ?_assertEqual(not_found, get_item(?LOCAL_CONTEXT, wocky_db:create_id(),
                                          ?ITEM)),
        ?_assertEqual(not_found, get_item(?LOCAL_CONTEXT, ?BOT,
                                          wocky_db:create_id()))
      ]}
    ]}.

test_publish_item() ->
    ID = <<"newID">>,
    Content = <<"New Content">>,
    { "publish_item", inorder, [
      { "publishes a new item when one doesn't exist", inorder, [
        ?_test(begin
                   #{updated := U} = get_bot(?LOCAL_CONTEXT, ?BOT),
                   ?assertEqual(ok, publish_item(?LOCAL_CONTEXT, ?BOT, ID,
                                                 Content, false)),
                   ?assertMatch(#{id := ID, stanza := Content},
                                get_item(?LOCAL_CONTEXT, ?BOT, ID)),
                   #{updated := U2} = get_bot(?LOCAL_CONTEXT, ?BOT),
                   ?assert(U < U2)
               end)
      ]},
      { "updates an existing item", inorder, [
        ?_test(begin
                   #{updated := U} = get_bot(?LOCAL_CONTEXT, ?BOT),
                   ?assertEqual(ok, publish_item(?LOCAL_CONTEXT, ?BOT, ID,
                                                 <<"Updated content">>, false)),
                   ?assertMatch(#{id := ID, stanza := <<"Updated content">>},
                                get_item(?LOCAL_CONTEXT, ?BOT, ID)),
                   #{updated := U2} = get_bot(?LOCAL_CONTEXT, ?BOT),
                   ?assertEqual(U, U2)
               end)
      ]}
    ]}.

test_delete_item() ->
    { "delete_item", [
      { "deletes an existing item", [
        ?_assertEqual(ok, delete_item(?LOCAL_CONTEXT, ?BOT, ?ITEM)),
        ?_assertEqual(not_found, get_item(?LOCAL_CONTEXT, ?BOT, ?ITEM))
      ]},
      { "does not fail on a non-existant item", [
        ?_assertEqual(ok, delete_item(?LOCAL_CONTEXT, ?BOT, <<"NotANoteID">>))
      ]}
    ]}.

test_subscribe_temporary() ->
    { "subscribe_temporary", [
      { "adds a temporary subscription", [
        ?_assertEqual(ok, unsubscribe(?LOCAL_CONTEXT, ?BOT, ?TIM_JID)),
        check_subscribers(base_subscribers()),
        ?_assertEqual(ok, subscribe_temporary(?LOCAL_CONTEXT, ?BOT,
                                              bobs_phone(), node())),
        check_subscribers([bobs_phone() | base_subscribers()])
      ]},
      { "does not add a device subscription if the user is alredy subscribed", [
        ?_assertEqual(ok, subscribe_temporary(?LOCAL_CONTEXT, ?BOT,
                                              device(?CAROL, <<"iphone">>),
                                              node())),
        check_subscribers([bobs_phone() | base_subscribers()])
      ]}
    ]}.

test_unsubscribe_temporary() ->
    { "unsubscribe_temporary", [
      { "has no effect on a device with no temporary subscription", [
        ?_assertEqual(ok, unsubscribe_temporary(?LOCAL_CONTEXT, ?BOT,
                                                device(?CAROL, <<"iphone">>))),
        check_subscribers([bobs_phone() | base_subscribers()])
      ]},
      { "removes a temporary subscription", [
        ?_assertEqual(ok, unsubscribe_temporary(?LOCAL_CONTEXT, ?BOT,
                                                bobs_phone())),
        check_subscribers(base_subscribers())
      ]}
    ]}.

test_clear_temporary_subscriptions() ->
    TimsPhone = device(?TIM, <<"abc">>),
    Bot2 = wocky_db:create_id(),
    { "clear_temporary_subscriptions", [
      { "clears all items temporarally subscribed from a node", [
        ?_assertEqual(ok, subscribe_temporary(?LOCAL_CONTEXT, ?BOT,
                                              bobs_phone(), node())),
        ?_assertEqual(ok, subscribe_temporary(?LOCAL_CONTEXT, ?BOT,
                                              TimsPhone, node())),
        check_subscribers([TimsPhone, bobs_phone() | base_subscribers()]),
        ?_assertEqual(ok, clear_temporary_subscriptions(node())),
        check_subscribers(base_subscribers())
      ]},
      { "clears all temporary subscriptions from a device", [
        ?_assertEqual(ok, subscribe_temporary(?LOCAL_CONTEXT, ?BOT,
                                              bobs_phone(), node())),
        ?_assertEqual(ok, subscribe_temporary(?LOCAL_CONTEXT, Bot2,
                                              bobs_phone(), node())),
        ?_assertEqual([bobs_phone()], subscribers(?LOCAL_CONTEXT, Bot2)),
        ?_assertEqual(ok, clear_temporary_subscriptions(bobs_phone())),
        check_subscribers(base_subscribers()),
        ?_assertEqual([], subscribers(?LOCAL_CONTEXT, Bot2))
      ]}
    ]}.

test_has_access() ->
    { "has_access", [
      { "returns true for everyone on public bots", [
        ?_assertEqual(ok, insert(?LOCAL_CONTEXT, #{id => ?BOT,
                                                   visibility =>
                                                   ?WOCKY_BOT_VIS_OPEN})),
        ?_assert(has_access(?LOCAL_CONTEXT, ?BOT, ?ALICE_JID)),
        ?_assert(has_access(?LOCAL_CONTEXT, ?BOT, ?BOB_JID)),
        ?_assert(has_access(?LOCAL_CONTEXT, ?BOT, ?KAREN_JID)),
        ?_assert(has_access(?LOCAL_CONTEXT, ?BOT, ?TIM_JID))
      ]},
      { "returns true for everyone to whom the bot is shared on private bots",
       [
        ?_test(
           begin
               ?assertEqual(ok, insert(?LOCAL_CONTEXT, #{id => ?BOT,
                                                         visibility => V})),
               ?assert(has_access(?LOCAL_CONTEXT, ?BOT, ?ALICE_JID)),
               ?assertEqual(ok, add_share(?ALICE_JID, ?BOB_JID, ?BOT_JID)),
               ?assert(has_access(?LOCAL_CONTEXT, ?BOT, ?BOB_JID)),
               ?assertNot(has_access(?LOCAL_CONTEXT, ?BOT, ?TIM_JID))
           end
              )
        || V <- [?WOCKY_BOT_VIS_OWNER,
                 ?WOCKY_BOT_VIS_WHITELIST,
                 ?WOCKY_BOT_VIS_FRIENDS]
      ]},
      { "returns not_found for non-existant bot", [
        ?_assertEqual(not_found, has_access(?LOCAL_CONTEXT,
                                            wocky_db:create_id(),
                                            ?BOB_JID))
      ]}
    ]}.

test_subscribe() ->
    Subscribers = [?TIM_JID, ?CAROL_JID, ?BOB_JID, ?KAREN_JID],
    { "subscribe", inorder, [
      { "adds user to list of subscribers", inorder, [
        ?_assertEqual(ok, subscribe(?LOCAL_CONTEXT, ?BOT, ?TIM_JID)),
        ?_assertEqual(ok, subscribe(?LOCAL_CONTEXT, ?BOT, ?BOB_JID)),
        check_subscribers(Subscribers)
      ]},
      { "does not alter the result if the owner is subscribed", inorder, [
        ?_assertEqual(ok, subscribe(?LOCAL_CONTEXT, ?BOT, ?ALICE_JID)),
        check_subscribers(Subscribers)
      ]}
    ]}.

test_unsubscribe() ->
    Subscribers = [?CAROL_JID, ?KAREN_JID],
    { "unsubscribe", [
      { "removes user from the list of subscribers", inorder, [
        ?_assertEqual(ok, unsubscribe(?LOCAL_CONTEXT, ?BOT, ?TIM_JID)),
        ?_assertEqual(ok, unsubscribe(?LOCAL_CONTEXT, ?BOT, ?BOB_JID)),
        check_subscribers(Subscribers)
      ]},
      { "does not alter the result if the owner is unsubscribed", inorder, [
        ?_assertEqual(ok, unsubscribe(?LOCAL_CONTEXT, ?BOT, ?ALICE_JID)),
        check_subscribers(Subscribers)
      ]}
    ]}.

check_subscribers(Subs) ->
    ?_assertEqual(lists:sort(Subs),
                  lists:sort(subscribers(?LOCAL_CONTEXT, ?BOT))).

test_delete() ->
    NewBot = #{id := ID} = new_bot(),
    { "delete", [
      { "deletes the specified bot and its shortname lookup, if present", [
        ?_assertEqual(ok, delete(?LOCAL_CONTEXT, ?BOT)),
        ?_assertEqual(not_found, get_bot(?LOCAL_CONTEXT, ?BOT)),
        ?_assertEqual(not_found, get_id_by_name(?LOCAL_CONTEXT, ?BOT))
      ]},
      { "deletes cleanly when no shortname exists", [
        ?_assertEqual(ok, insert(?LOCAL_CONTEXT,
                                 maps:without([shortname], NewBot))),
        ?_assertEqual(ok, delete(?LOCAL_CONTEXT, ID)),
        ?_assertEqual(not_found, get_bot(?LOCAL_CONTEXT, ID))
      ]},
      { "does not fail on invalid ID", [
        ?_assertEqual(ok, delete(?LOCAL_CONTEXT, wocky_db:create_id()))
      ]}
    ]}.

new_bot() ->
    #{id => wocky_db:create_id(), server => ?LOCAL_CONTEXT,
      title => <<"Test bot X">>, shortname => <<"ShortName">>,
      owner => ?BOB_B_JID, description => <<"Test insert bot">>,
      address => <<"test address">>, lat => 1.0, lon => -2.0, radius => 10,
      visibility => ?WOCKY_BOT_VIS_OWNER,
      alerts => ?WOCKY_BOT_ALERT_ENABLED,
      image => <<>>, type => <<"killbot">>,
      tags => ?BOT_TAGS,
      follow_me => false, follow_me_expiry => 0
     }.

expected_item() ->
    hd(wocky_db_seed:seed_data(bot_item, none)).

device(User, Resource) -> jid:make(User, ?LOCAL_CONTEXT, Resource).

base_subscribers() -> [?CAROL_JID, ?KAREN_JID].

bobs_phone() -> device(?BOB, <<"phone">>).
