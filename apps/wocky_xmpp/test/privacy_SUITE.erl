%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(privacy_SUITE).
-compile(export_all).
-compile({parse_transform, fun_chain}).
-compile({parse_transform, cut}).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

-define(SLEEP_TIME, 50).

-include("wocky_db_seed.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, management},
     {group, blocking},
     {group, allowing},
     {group, wocky}
    ].

groups() ->
    [{management, [sequence], management_test_cases()},
     {blocking,   [sequence], blocking_test_cases()},
     {allowing,   [sequence], allowing_test_cases()},
     {wocky,      [sequence], wocky_test_cases()}
    ].

management_test_cases() ->
    [get_all_lists,
     get_existing_list,
     get_many_lists,
     get_nonexistent_list,
     set_list,
     activate,
     activate_nonexistent,
     deactivate,
     default,
     default_nonexistent,
     no_default,
     remove_list,
     get_all_lists_with_active,
     get_all_lists_with_default
    ].

blocking_test_cases() ->
    [block_jid_message,
     block_group_message,
     block_subscription_message,
     block_all_message,
     block_jid_presence_in,
     block_jid_presence_out,
     block_jid_iq,
     block_jid_all,
     block_jid_message_but_not_presence
    ].

allowing_test_cases() ->
    [allow_subscription_to_from_message,
     allow_subscription_both_message].

wocky_test_cases() ->
    [default_privacy_list].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

users() -> [alice, bob, carol, tim].

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),
    escalus:init_per_suite([{escalus_no_stanzas_after_story, true} | Config]).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    test_helper:setup_users(Config, users()).

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

%% In terms of server response to blocked communication, we strive to implement
%% the following as defined in XEP-0016:
%% If someone I block tries to communicate with me, then the following rules
%% apply:
%%  * For presence stanzas (including notifications, subscriptions, and probes),
%%  the server MUST NOT respond and MUST NOT return an error.
%%  * For message stanzas, the server SHOULD return an error, which SHOULD be
%%  <service-unavailable/>.
%%  * For IQ stanzas of type "get" or "set", the server MUST return an error,
%%  which SHOULD be <service-unavailable/>. IQ stanzas of other types MUST be
%%  silently dropped by the server.
%% If I want to communicate with someone I block, then:
%%  * If the user attempts to send an outbound stanza to a contact and that
%%  stanza type is blocked, the user's server MUST NOT route the stanza to the
%%  contact but instead MUST return a <not-acceptable/> error:


%% TODO:
%% x get all privacy lists
%% x get single privacy list
%%   x that exists
%%   x that doesn't exist (ensure server returns item-not-found)
%%   x request more than one at a time (ensure server returns bad-request)
%% x set new/edit privacy list (ensure server pushes notifications
%%   to all resources)
%% - remove existing list
%%   x remove existing list (ensure server push)
%%   - remove, but check conflict case
%% x manage active list(s)
%%   x activate
%%   x activate nonexistent (ensure item-not-found)
%%   x deactivate by sending empty <active />
%% - manage default list
%%   x set default
%%   - set default, but check the conflict case, i.e.:
%%     "Client attempts to change the default list but that list is in use
%%     by another resource",
%%     !!! ejabberd doesn't support this, bug filed (#7073)
%%   x set nonexistent default list
%%   x use domain's routing, i.e. no default list -> send empty <default />
%%   - set no default list, but check conflict case,
%%     when a resource currently uses the default list
%%
%% TODO later:
%% - big picture:
%%   - blocking can be done on jids, roster groups,
%%     subscription type or globally
%%   - a blocking rule may block one or more of {message, presence-in,
%%     presence-out, iqs} by specifying these as children to the list item
%%     or block all of them, when the item has no children
%% - blocking: messages, presence (in/out), iqs, all

get_all_lists(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->

        escalus:send(Alice, escalus_stanza:privacy_get_all()),
        escalus:assert(is_privacy_result, escalus:wait_for_stanza(Alice))

        end).

get_all_lists_with_active(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, _Bob) ->

        privacy_helper:set_and_activate(Alice, <<"deny_bob">>),

        escalus:send(Alice, escalus_stanza:privacy_get_all()),
        escalus:assert(is_privacy_result_with_active, [<<"deny_bob">>],
                       escalus:wait_for_stanza(Alice))

        end).

get_all_lists_with_default(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, _Bob) ->

        privacy_helper:set_list(Alice, <<"deny_bob">>),
        privacy_helper:set_list(Alice, <<"allow_bob">>),

        escalus:send(Alice, escalus_stanza:privacy_get_all()),
        escalus:assert(is_privacy_result_with_default, [<<"default">>],
                       escalus:wait_for_stanza(Alice))

        end).

get_nonexistent_list(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->

        escalus_client:send(Alice,
            escalus_stanza:privacy_get_lists([<<"public">>])),
        escalus_assert:is_privacy_list_nonexistent_error(
            escalus_client:wait_for_stanza(Alice))

        end).

get_many_lists(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->

        Request = escalus_stanza:privacy_get_lists([<<"public">>,
                                                    <<"private">>]),
        escalus_client:send(Alice, Request),
        Response = escalus_client:wait_for_stanza(Alice),
        escalus_assert:is_error(Response, <<"modify">>, <<"bad-request">>)

        end).

get_existing_list(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, _Bob) ->

        privacy_helper:set_list(Alice, <<"deny_bob">>),

        escalus:send(Alice, escalus_stanza:privacy_get_lists([<<"deny_bob">>])),
        Response = escalus:wait_for_stanza(Alice),

        <<"deny_bob">> = exml_query:path(Response, [{element, <<"query">>},
                                                    {element, <<"list">>},
                                                    {attr, <<"name">>}])

        end).

activate(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, _Bob) ->

        privacy_helper:set_list(Alice, <<"deny_bob">>),

        Request = escalus_stanza:privacy_activate(<<"deny_bob">>),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, Response)

        end).

activate_nonexistent(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->

        Request = escalus_stanza:privacy_activate(<<"some_list">>),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_error, [<<"cancel">>, <<"item-not-found">>], Response)

        end).

deactivate(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->

        Request = escalus_stanza:privacy_deactivate(),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, Response)

        end).

default(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, _Bob) ->

        privacy_helper:set_list(Alice, <<"deny_bob">>),

        Request = escalus_stanza:privacy_set_default(<<"deny_bob">>),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),

        %% Setting the default is not allowed by wocky
        escalus:assert(is_iq_error, Response)

        end).

default_nonexistent(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->

        Request = escalus_stanza:privacy_set_default(<<"some_list">>),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),
        escalus_assert:is_error(Response, <<"cancel">>, <<"item-not-found">>)

        end).

no_default(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->

        Request = escalus_stanza:privacy_no_default(),
        escalus_client:send(Alice, Request),

        Response = escalus_client:wait_for_stanza(Alice),
        %% Setting the default is not allowed by wocky
        escalus:assert(is_iq_error, Response)

        end).

set_list(Config) ->
    escalus:story(Config, [{alice, 3}, {bob, 1}],
                  fun(Alice, Alice2, Alice3, _Bob) ->

        privacy_helper:send_set_list(Alice, <<"deny_bob">>),

        %% Verify that original Alice gets iq result and notification.
        %% It's a bit quirky as these come in undefined order
        %% (actually, they always came with 'push' first and then 'result',
        %% but I suppose it's not mandatory).
        AliceResponses = escalus_client:wait_for_stanzas(Alice, 2),
        escalus:assert_many([
            fun escalus_pred:is_iq_result/1,
            fun privacy_helper:is_privacy_list_push/1
        ], AliceResponses),

        %% Verify that other resources also get the push.
        escalus:assert(fun privacy_helper:is_privacy_list_push/1,
                       escalus:wait_for_stanza(Alice2)),
        escalus:assert(fun privacy_helper:is_privacy_list_push/1,
                       escalus:wait_for_stanza(Alice3))

        %% All in all, the spec requires the resources to reply
        %% (as to every iq), but it's omitted here.

        end).

remove_list(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, _Bob) ->

        privacy_helper:send_set_list(Alice, <<"deny_bob">>),

        %% These are the pushed notification and iq result.
        escalus_client:wait_for_stanzas(Alice, 2),

        %% Request list deletion by sending an empty list.
        RemoveRequest = escalus_stanza:privacy_set_list(
                escalus_stanza:privacy_list(<<"someList">>, [])),
        escalus_client:send(Alice, RemoveRequest),

        %% These too are the pushed notification and iq result.
        escalus:assert_many([
            fun privacy_helper:is_privacy_list_push/1,
            is_iq_result
        ], escalus_client:wait_for_stanzas(Alice, 2)),

        escalus_client:send(Alice,
            escalus_stanza:privacy_get_lists([<<"someList">>])),

        %% Finally ensure that the list doesn't exist anymore.
        escalus_assert:is_privacy_list_nonexistent_error(
            escalus_client:wait_for_stanza(Alice))

        end).

block_jid_message(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        clear_default_list([Alice, Bob]),

        %% Alice should receive message
        can_chat_to(Bob, Alice),

        %% set the list on server and make it active
        privacy_helper:set_and_activate(Alice, <<"deny_bob_message">>),

        %% Alice should NOT receive message, while Bob gets error message
        can_not_chat_to(Bob, Alice),

        %% Message blocking is only for incoming, so Alice can still
        %% send to Bob
        can_chat_to(Alice, Bob)

        end).

block_group_message(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        clear_default_list([Alice, Bob]),

        %% Alice should receive message
        can_chat_to(Bob, Alice),

        %% add Bob to Alices' group 'ignored'
        test_helper:add_contact(Alice, Bob, [<<"ignored">>],
                                <<"Ugly Bastard">>),

        %% set the list on server and make it active
        privacy_helper:set_and_activate(Alice, <<"deny_group_message">>),

        %% Alice should NOT receive message
        can_not_chat_to(Bob, Alice)

        end).

block_subscription_message(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        clear_default_list([Alice, Bob]),

        %% Alice should receive message
        can_chat_to(Bob, Alice),

        %% Alice sends unsubscribe
        escalus_client:send(Alice,
            escalus_stanza:presence_direct(?BOB_B_JID, <<"unsubscribe">>)),

        %% set the list on server and make it active
        privacy_helper:set_and_activate(Alice,
                                        <<"deny_unsubscribed_message">>),

        %% Alice should NOT receive message
        can_not_chat_to(Bob, Alice)

        end).

allow_subscription_to_from_message(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        %% deny all message but not from subscribed "to"
        privacy_helper:set_and_activate(
          Alice, <<"deny_all_message_but_subscription_to">>),

        %% deny all message but not from subscribed "from"
        privacy_helper:set_and_activate(
          Bob, <<"deny_all_message_but_subscription_from">>),

        %% Bob and Alice cannot sent to each other now
        can_not_chat_to(Bob, Alice),
        can_not_chat_to(Alice, Bob),

        %% Alice subscribes to Bob
        escalus_client:send(Alice,
                            escalus_stanza:presence_direct(
                              ?BOB_B_JID, <<"subscribe">>)),
        escalus_client:wait_for_stanza(Alice),
        escalus_client:wait_for_stanza(Bob),

        %% Bob accepts Alice
        escalus_client:send(Bob, escalus_stanza:presence_direct(
                                   ?ALICE_B_JID, <<"subscribed">>)),
        escalus_client:wait_for_stanza(Bob),
        escalus_client:wait_for_stanzas(Alice, 3),

        %% Now Alice is subscirbed "to" Bob
        %% And Bob is subscribed "from" Alice
        can_chat_to(Bob, Alice),
        can_chat_to(Alice, Bob)
    end).


allow_subscription_both_message(Config) ->
    Config2 = test_helper:setup_users(Config, users()),
    escalus:story(Config2, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        %% deny all message but not from subscribed "both"
        privacy_helper:set_and_activate(
          Alice, <<"deny_all_message_but_subscription_both">>),

        %% deny all message but not from subscribed "both"
        privacy_helper:set_and_activate(
          Bob, <<"deny_all_message_but_subscription_both">>),

        %% Bob and Alice cannot sent to each other now
        %% Even though they are in subscription "to" and "from" respectively
        can_not_chat_to(Alice, Bob),
        can_not_chat_to(Bob, Alice),

        %% Alice and Bob subscribe to eachother
        test_helper:subscribe(Alice, Bob),
        %% Because we're not on the default wocky privacy lists, Alice *will*
        %% get a presence stanza here (so we can't use
        %% test_helper:subscribe_pair)
        escalus:assert(is_presence, escalus_client:wait_for_stanza(Alice)),
        test_helper:subscribe(Bob, Alice),
        escalus:assert(is_presence, escalus_client:wait_for_stanza(Bob)),

        %% Now their subscription is in state "both"
        can_chat_to(Bob, Alice),
        can_chat_to(Alice, Bob)

    end).

block_all_message(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        clear_default_list([Alice, Bob]),

        %% Alice should receive message
        can_chat_to(Bob, Alice),

        %% set the list on server and make it active
        privacy_helper:set_and_activate(Alice, <<"deny_all_message">>),

        %% Alice should NOT receive message
        can_not_chat_to(Bob, Alice)

        end).

block_jid_presence_in(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        clear_default_list([Alice, Bob]),

        %% Alice should receive presence in
        escalus_client:send(Bob,
            escalus_stanza:presence_direct(?ALICE_B_JID, <<"available">>)),
        Received = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_presence, Received),
        escalus_assert:is_stanza_from(Bob, Received),

        privacy_helper:set_and_activate(Alice, <<"deny_bob_presence_in">>),

        %% Alice should NOT receive presence in
        escalus_client:send(Bob,
            escalus_stanza:presence_direct(?ALICE_B_JID, <<"available">>)),
        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Alice),
        %% and Bob should NOT receive any response
        escalus_assert:has_no_stanzas(Bob)


        end).

block_jid_presence_out(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        clear_default_list([Alice, Bob]),

        %% Bob should receive presence in
        escalus_client:send(Alice,
            escalus_stanza:presence_direct(?BOB_B_JID, <<"available">>)),
        Received = escalus_client:wait_for_stanza(Bob),
        escalus:assert(is_presence, Received),
        escalus_assert:is_stanza_from(Alice, Received),

        privacy_helper:activate_list(Alice, <<"default">>),

        %% Bob should NOT receive presence in
        escalus_client:send(Alice,
            escalus_stanza:presence_direct(?BOB_B_JID, <<"available">>)),

        %% Alice gets an error back from mod_privacy
        Presence = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_presence_with_type, [<<"error">>], Presence),

        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Bob)

        end).

version_iq(Type, From, To) ->
    Req = escalus_stanza:iq(
            Type, [escalus_stanza:query_el(<<"jabber:iq:version">>, [])]),
    Req1 = escalus_stanza:to(Req, To),
    Req2= escalus_stanza:from(Req1, From),
    Req2.

block_jid_iq(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        privacy_helper:set_list(Alice, <<"deny_localhost_iq">>),
        %% activate it
        escalus_client:send(Alice,
            escalus_stanza:privacy_activate(<<"deny_localhost_iq">>)),
        timer:sleep(500), %% we must let it sink in
        %% bob queries for version and gets an error, Alice doesn't receive
        %% the query
        escalus_client:send(Bob, version_iq(<<"get">>, Bob, Alice)),
        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Alice),
        privacy_helper:gets_error(Bob, <<"service-unavailable">>),
        %% this stanza does not make much sense, but is routed and rejected
        %% correctly
        escalus_client:send(Bob, version_iq(<<"set">>, Bob, Alice)),
        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Alice),
        privacy_helper:gets_error(Bob, <<"service-unavailable">>),
        %% but another type, like result, is silently dropped
        escalus_client:send(Bob, version_iq(<<"result">>, Bob, Alice)),
        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Alice),
        escalus_assert:has_no_stanzas(Bob)

        end).

block_jid_all(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        %% Override wocky's default blocking
        privacy_helper:set_and_activate(Bob, <<"allow_alice">>),

        privacy_helper:set_list(Alice, <<"deny_jid_all">>),

        %% Alice blocks Bob
        escalus_client:send(Alice,
            escalus_stanza:privacy_activate(<<"deny_jid_all">>)),
        %% IQ response is blocked;
        %% do magic wait for the request to take effect
        timer:sleep(200),

        %% From now on nothing whatsoever sent by Bob should reach Alice.

        %% Alice should NOT receive message, Bob receives err msg
        can_not_chat_to(Bob, Alice),

        %% Alice should NOT receive presence-in from Bob, no err msg
        escalus_client:send(Bob,
            escalus_stanza:presence_direct(?ALICE_B_JID, <<"available">>)),
        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Bob),

        %% Bob should NOT receive presence-in from Alice, Alice receives err msg
        escalus_client:send(Alice,
            escalus_stanza:presence_direct(?BOB_B_JID, <<"available">>)),
        timer:sleep(?SLEEP_TIME),
        privacy_helper:gets_error(Alice, <<"not-acceptable">>),

        %% Just set the toy list and ensure that only
        %% the notification push comes back.
        privacy_helper:send_set_list(Alice, <<"deny_bob">>),

        %% verify
        timer:sleep(?SLEEP_TIME),
        %% ...that nothing else reached Bob
        escalus_assert:has_no_stanzas(Bob),
        %% ...that Alice got a privacy push
        Responses = escalus_client:wait_for_stanza(Alice),
        escalus:assert(fun privacy_helper:is_privacy_list_push/1, Responses),
        %% and Alice didn't get anything else
        escalus_assert:has_no_stanzas(Alice)

        end).

block_jid_message_but_not_presence(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        clear_default_list([Alice, Bob]),

        %% Alice should receive message
        can_chat_to(Bob, Alice),

        %% set the list on server and make it active
        privacy_helper:set_and_activate(Alice, <<"deny_bob_message">>),

        %% Alice should NOT receive message
        can_not_chat_to(Bob, Alice),

        %% ...but should receive presence in
        escalus_client:send(Bob,
            escalus_stanza:presence_direct(?ALICE_B_JID, <<"available">>)),
        Received = escalus_client:wait_for_stanza(Alice),
        escalus:assert(is_presence, Received),
        escalus_assert:is_stanza_from(Bob, Received)

        end).

default_privacy_list(Config) ->
    wocky_db:truncate(shared, roster),
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}, {tim, 1}],
      fun(Alice, Bob, Carol, Tim) ->
        test_helper:subscribe_pair(Alice, Bob),
        test_helper:subscribe(Carol, Alice),

        %% Alice and bob are friends so can chat in both directions
        can_chat_to(Alice, Bob),
        can_chat_to(Bob, Alice),

        %% Alice can chat to Carol, her follower, but not vice-versa
        can_chat_to(Alice, Carol),
        can_not_chat_to(Carol, Alice),

        %% Alice and Tim have no relationship, so can't chat to each other
        can_not_chat_to(Alice, Tim),
        can_not_chat_to(Tim, Alice)
      end).


%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------

can_chat_to(Who, Whom) ->
    Msg = <<"Hi! What's your name?">>,
    escalus_client:send(Who, escalus_stanza:chat_to( Whom, Msg)),
    escalus_assert:is_chat_message(Msg, escalus_client:wait_for_stanza(Whom)).

can_not_chat_to(Who, Whom) ->
    escalus_client:send(Who, escalus_stanza:chat_to(Whom, <<"Hi!">>)),
    timer:sleep(?SLEEP_TIME),
    escalus_assert:has_no_stanzas(Whom),
    privacy_helper:gets_error(Who, <<"service-unavailable">>).

clear_default_list(Clients) ->
    lists:foreach(
        privacy_helper:set_and_activate(_, <<"allow_all">>),
        Clients).
