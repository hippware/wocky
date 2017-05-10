%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite for multicast addressing
-module(multicast_SUITE).
-compile(export_all).
-compile({parse_transform, fun_chain}).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("wocky_db_seed.hrl").

-compile({parse_transform, cut}).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() -> [friends,
          followers,
          to,
          mix,
          error
         ].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

users() ->
    [alice, bob, carol, karen, robert, tim].

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),
    fun_chain:first(Config,
        escalus:init_per_suite(),
        test_helper:setup_users(users())
    ).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

friends(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1},
                           {robert, 1}, {karen, 1}, {tim, 1}],
      fun(Alice, Bob, Carol, Robert, Karen, Tim) ->
        %% Make Alice friends with Bob and Carol
        lists:foreach(
          test_helper:subscribe_pair(_, Alice),
          [Bob, Carol]),

        %% Make Tim a follower of Alice
        test_helper:subscribe(Tim, Alice),

        SendStanza = multicast_stanza(<<"friends">>),

        escalus:send(Alice, SendStanza),

        expect_multicast_msg([Bob, Carol]),

        %% Tim is not a friend, so gets nothing
        test_helper:ensure_all_clean([Robert, Karen, Tim])
    end).

followers(Config) ->
    ?wocky_repo:delete_all(?wocky_roster_item),
    escalus:story(Config, [{alice, 1}, {bob, 1}, {tim, 1}, {robert, 1}],
      fun(Alice, Bob, Tim, Robert) ->
        test_helper:subscribe_pair(Bob, Alice),
        test_helper:subscribe(Tim, Alice),
        SendStanza = multicast_stanza(<<"followers">>),

        escalus:send(Alice, SendStanza),

        expect_multicast_msg([Bob, Tim]),
        test_helper:ensure_all_clean([Robert])
      end).

to(Config) ->
    ?wocky_repo:delete_all(?wocky_roster_item),
    escalus:story(Config, [{alice, 1}, {bob, 1}, {tim, 1}, {robert, 1}],
      fun(Alice, Bob, Tim, Robert) ->
        test_helper:subscribe_pair(Bob, Alice),
        SendStanza = multicast_stanza([[{<<"type">>, <<"to">>},
                                        {<<"jid">>, ?BOB_B_JID}]],
                                      ?NS_ADDRESS),

        escalus:send(Alice, SendStanza),

        expect_multicast_msg([Bob]),
        test_helper:ensure_all_clean([Tim, Robert])
      end).

mix(Config) ->
    ?wocky_repo:delete_all(?wocky_roster_item),
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1},
                           {tim, 1}, {robert, 1}],
      fun(Alice, Bob, Carol, Tim, Robert) ->
        test_helper:subscribe_pair(Robert, Alice),
        test_helper:subscribe_pair(Bob, Alice),
        test_helper:subscribe(Tim, Alice),
        SendStanza = multicast_stanza([[{<<"type">>, <<"followers">>}],
                                       [{<<"type">>, <<"friends">>}],
                                       [{<<"type">>, <<"to">>},
                                        {<<"jid">>,  ?ROBERT_B_JID}]],
                                      ?NS_ADDRESS),

        escalus:send(Alice, SendStanza),

        expect_multicast_msg([Bob, Tim, Robert]),
        test_helper:ensure_all_clean([Carol])
      end).

error(Config) ->
    ?wocky_repo:delete_all(?wocky_roster_item),
    escalus:story(Config, [{alice, 1}, {bob, 1}, {tim, 1}],
      fun(Alice, Bob, Tim) ->
        test_helper:subscribe_pair(Bob, Alice),
        test_helper:subscribe(Tim, Alice),

        %% Unknown multicast address
        escalus:send(Alice, multicast_stanza(<<"fnord">>)),

        %% Wrong NS
        escalus:send(Alice, multicast_stanza([[{<<"type">>, <<"friends">>}]],
                                             <<"wrongNS">>)),

        %% Invalid jid
        escalus:send(Alice, multicast_stanza([[{<<"type">>, <<"to">>},
                                               {<<"jid">>, <<"@/@/">>}]],
                                             ?NS_ADDRESS)),

        %% Missing jid
        escalus:send(Alice, multicast_stanza([[{<<"type">>, <<"to">>}]],
                                             ?NS_ADDRESS)),

        %% Missing type
        escalus:send(Alice, multicast_stanza([[]], ?NS_ADDRESS)),

        test_helper:ensure_all_clean([Alice, Bob, Tim])
      end).

multicast_stanza(Type) ->
    multicast_stanza([[{<<"type">>, Type}]], ?NS_ADDRESS).

multicast_stanza(AddressAttrs, NS) ->
    Stanza = escalus_stanza:chat_to(
               ?SERVER, <<"Check out my LOLCAT">>),
    AddressEls = make_address_els(AddressAttrs),
    Addresses = #xmlel{name = <<"addresses">>,
                       attrs = [{<<"xmlns">>, NS}],
                       children = AddressEls},
    Stanza#xmlel{children =
                 [Addresses | Stanza#xmlel.children]}.

make_address_els(AddressAttrsList) ->
    lists:map(
      fun(AA) -> #xmlel{name = <<"address">>, attrs = AA} end,
      AddressAttrsList).

expect_multicast_msg(Recipients) ->
    lists:foreach(
      fun(C) ->
        RecStanza = escalus_client:wait_for_stanza(C),
        ?assertEqual(false,
                     xml:get_subtag(RecStanza, <<"addresses">>))
      end, Recipients).
