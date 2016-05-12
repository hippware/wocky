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
     {group, allowing}
    ].

groups() ->
    [{management, [], management_test_cases()},
     {blocking, [], blocking_test_cases()},
     {allowing, [], allowing_test_cases()}
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
     %default_conflict,  % fails, as of bug #7073
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


suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),
    wocky_db_seed:clear_user_tables(?LOCAL_CONTEXT),
    wocky_db_seed:clear_tables(?LOCAL_CONTEXT, [privacy, privacy_item]),
    Config1 = escalus:create_users(Config, escalus:get_users([carol, karen])),
    [{escalus_no_stanzas_after_story, true} |
     escalus:init_per_suite(Config1)].

end_per_suite(Config) ->
    escalus:delete_users(Config, escalus:get_users([carol, karen])),
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

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
    escalus:story(Config, [{carol, 1}, {karen, 1}], fun(Carol, _Karen) ->

        escalus:send(Carol, escalus_stanza:privacy_get_all()),
        escalus:assert(is_privacy_result, escalus:wait_for_stanza(Carol))

        end).

get_all_lists_with_active(Config) ->
    escalus:story(Config, [{carol, 1}, {karen, 1}], fun(Carol, _Karen) ->

        privacy_helper:set_and_activate(Carol, <<"deny_karen">>),

        escalus:send(Carol, escalus_stanza:privacy_get_all()),
        escalus:assert(is_privacy_result_with_active, [<<"deny_karen">>],
                       escalus:wait_for_stanza(Carol))

        end).

get_all_lists_with_default(Config) ->
    escalus:story(Config, [{carol, 1}, {karen, 1}], fun(Carol, _Karen) ->

        privacy_helper:set_list(Carol, <<"deny_karen">>),
        privacy_helper:set_and_activate(Carol, <<"allow_karen">>),

        escalus:send(Carol, escalus_stanza:privacy_get_all()),
        escalus:assert(is_privacy_result_with_default,
                       escalus:wait_for_stanza(Carol))

        end).

get_nonexistent_list(Config) ->
    escalus:story(Config, [{carol, 1}], fun(Carol) ->

        escalus_client:send(Carol,
            escalus_stanza:privacy_get_lists([<<"public">>])),
        escalus_assert:is_privacy_list_nonexistent_error(
            escalus_client:wait_for_stanza(Carol))

        end).

get_many_lists(Config) ->
    escalus:story(Config, [{carol, 1}], fun(Carol) ->

        Request = escalus_stanza:privacy_get_lists([<<"public">>,
                                                    <<"private">>]),
        escalus_client:send(Carol, Request),
        Response = escalus_client:wait_for_stanza(Carol),
        escalus_assert:is_error(Response, <<"modify">>, <<"bad-request">>)

        end).

get_existing_list(Config) ->
    escalus:story(Config, [{carol, 1}], fun(Carol) ->
        privacy_helper:set_list(Carol, <<"deny_karen">>),

        escalus:send(Carol,
                     escalus_stanza:privacy_get_lists([<<"deny_karen">>])),
        Response = escalus:wait_for_stanza(Carol),

        <<"deny_karen">> = exml_query:path(Response, [{element, <<"query">>},
                                                    {element, <<"list">>},
                                                    {attr, <<"name">>}])

        end).

activate(Config) ->
    escalus:story(Config, [{carol, 1}, {karen, 1}], fun(Carol, _Karen) ->

        privacy_helper:set_list(Carol, <<"deny_karen">>),

        Request = escalus_stanza:privacy_activate(<<"deny_karen">>),
        escalus_client:send(Carol, Request),

        Response = escalus_client:wait_for_stanza(Carol),
        escalus:assert(is_iq_result, Response)

        end).

activate_nonexistent(Config) ->
    escalus:story(Config, [{carol, 1}], fun(Carol) ->

        Request = escalus_stanza:privacy_activate(<<"some_list">>),
        escalus_client:send(Carol, Request),

        Response = escalus_client:wait_for_stanza(Carol),
        escalus:assert(is_error, [<<"cancel">>, <<"item-not-found">>], Response)

        end).

deactivate(Config) ->
    escalus:story(Config, [{carol, 1}], fun(Carol) ->

        Request = escalus_stanza:privacy_deactivate(),
        escalus_client:send(Carol, Request),

        Response = escalus_client:wait_for_stanza(Carol),
        escalus:assert(is_iq_result, Response)

        end).

default(Config) ->
    escalus:story(Config, [{carol, 1}, {karen, 1}], fun(Carol, _Karen) ->

        privacy_helper:set_list(Carol, <<"deny_karen">>),

        Request = escalus_stanza:privacy_set_default(<<"deny_karen">>),
        escalus_client:send(Carol, Request),

        Response = escalus_client:wait_for_stanza(Carol),
        escalus:assert(is_iq_result, Response)

        end).

default_conflict(Config) ->
    escalus:story(Config, [{carol, 2}, {karen, 1}],
                  fun(Carol, Carol2, _Karen) ->

        %% testcase setup
        %% setup list on server
        privacy_helper:send_set_list(Carol, <<"deny_karen">>),
        privacy_helper:send_set_list(Carol, <<"allow_karen">>),
        %% skip responses
        escalus_client:wait_for_stanzas(Carol, 4),
        %% make a default list for Carol2
        R1 = escalus_stanza:privacy_set_default(<<"deny_karen">>),
        escalus_client:send(Carol2, R1),
        escalus:assert_many([is_privacy_set, is_privacy_set, is_iq_result],
                            escalus_client:wait_for_stanzas(Carol2, 3)),
        %% setup done

        Request = escalus_stanza:privacy_set_default(<<"allow_karen">>),
        escalus_client:send(Carol, Request),

        Response = escalus_client:wait_for_stanza(Carol),
        %% TODO: should fail on this (result) and receive error
        %%       this is a bug and was filed to the esl redmine as Bug #7073
        %%escalus:assert(is_iq_result, Response)
        %% but this should pass just fine
        escalus:assert(is_error, [<<"cancel">>, <<"conflict">>], Response)

        end).

default_nonexistent(Config) ->
    escalus:story(Config, [{carol, 1}], fun(Carol) ->

        Request = escalus_stanza:privacy_set_default(<<"some_list">>),
        escalus_client:send(Carol, Request),

        Response = escalus_client:wait_for_stanza(Carol),
        escalus_assert:is_error(Response, <<"cancel">>, <<"item-not-found">>)

        end).

no_default(Config) ->
    escalus:story(Config, [{carol, 1}], fun(Carol) ->

        Request = escalus_stanza:privacy_no_default(),
        escalus_client:send(Carol, Request),

        Response = escalus_client:wait_for_stanza(Carol),
        escalus:assert(is_iq_result, Response)

        end).

set_list(Config) ->
    escalus:story(Config, [{carol, 3}, {karen, 1}],
                  fun(Carol, Carol2, Carol3, _Karen) ->

        privacy_helper:send_set_list(Carol, <<"deny_karen">>),

        %% Verify that original Carol gets iq result and notification.
        %% It's a bit quirky as these come in undefined order
        %% (actually, they always came with 'push' first and then 'result',
        %% but I suppose it's not mandatory).
        CarolResponses = escalus_client:wait_for_stanzas(Carol, 2),
        escalus:assert_many([
            fun escalus_pred:is_iq_result/1,
            fun privacy_helper:is_privacy_list_push/1
        ], CarolResponses),

        %% Verify that other resources also get the push.
        escalus:assert(fun privacy_helper:is_privacy_list_push/1,
                       escalus:wait_for_stanza(Carol2)),
        escalus:assert(fun privacy_helper:is_privacy_list_push/1,
                       escalus:wait_for_stanza(Carol3))

        %% All in all, the spec requires the resources to reply
        %% (as to every iq), but it's omitted here.

        end).

remove_list(Config) ->
    escalus:story(Config, [{carol, 1}, {karen, 1}], fun(Carol, _Karen) ->

        privacy_helper:send_set_list(Carol, <<"deny_karen">>),

        %% These are the pushed notification and iq result.
        escalus_client:wait_for_stanzas(Carol, 2),

        %% Request list deletion by sending an empty list.
        RemoveRequest = escalus_stanza:privacy_set_list(
                escalus_stanza:privacy_list(<<"someList">>, [])),
        escalus_client:send(Carol, RemoveRequest),

        %% These too are the pushed notification and iq result.
        escalus:assert_many([
            fun privacy_helper:is_privacy_list_push/1,
            is_iq_result
        ], escalus_client:wait_for_stanzas(Carol, 2)),

        escalus_client:send(Carol,
            escalus_stanza:privacy_get_lists([<<"someList">>])),

        %% Finally ensure that the list doesn't exist anymore.
        escalus_assert:is_privacy_list_nonexistent_error(
            escalus_client:wait_for_stanza(Carol))

        end).

block_jid_message(Config) ->
    escalus:story(Config, [{carol, 1}, {karen, 1}], fun(Carol, Karen) ->

        %% Carol should receive message
        escalus_client:send(Karen,
            escalus_stanza:chat_to(Carol, <<"Hi! What's your name?">>)),
        escalus_assert:is_chat_message(<<"Hi! What's your name?">>,
            escalus_client:wait_for_stanza(Carol)),

        %% set the list on server and make it active
        privacy_helper:set_and_activate(Carol, <<"deny_karen_message">>),

        %% Carol should NOT receive message
        escalus_client:send(Karen, escalus_stanza:chat_to(Carol,
                                                        <<"Hi, Carol!">>)),
        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Carol)

        end).

block_group_message(Config) ->
    escalus:story(Config, [{carol, 1}, {karen, 1}], fun(Carol, Karen) ->

        %% Carol should receive message
        escalus_client:send(Karen, escalus_stanza:chat_to(Carol, <<"Hi!">>)),
        escalus_assert:is_chat_message(<<"Hi!">>,
            escalus_client:wait_for_stanza(Carol)),

        %% add Karen to Carols' group 'ignored'
        add_sample_contact(Carol, Karen, [<<"ignored">>], <<"Ugly Bastard">>),

        %% set the list on server and make it active
        privacy_helper:set_and_activate(Carol, <<"deny_group_message">>),

        %% Carol should NOT receive message
        escalus_client:send(Karen,
                            escalus_stanza:chat_to(Carol,
                                                   <<"Hi, blocked group!">>)),
        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Carol)

        end).

block_subscription_message(Config) ->
    escalus:story(Config, [{carol, 1}, {karen, 1}], fun(Carol, Karen) ->

        %% Carol should receive message
        escalus_client:send(Karen, escalus_stanza:chat_to(Carol, <<"Hi!">>)),
        escalus_assert:is_chat_message(<<"Hi!">>,
            escalus_client:wait_for_stanza(Carol)),

        %% Carol sends unsubscribe
        escalus_client:send(Carol,
            escalus_stanza:presence_direct(karen, <<"unsubscribe">>)),

        %% set the list on server and make it active
        privacy_helper:set_and_activate(Carol, <<"deny_unsubscribed_message">>),

        %% Carol should NOT receive message
        escalus_client:send(Karen, escalus_stanza:chat_to(Carol, <<"Hi!">>)),
        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Carol)

        end).

allow_subscription_to_from_message(Config) ->
    escalus:story(Config, [{carol, 1}, {karen, 1}], fun(Carol, Karen) ->
        %% deny all message but not from subscribed "to"
        privacy_helper:set_and_activate(
          Carol, <<"deny_all_message_but_subscription_to">>),

        %% deny all message but not from subscribed "from"
        privacy_helper:set_and_activate(
          Karen, <<"deny_all_message_but_subscription_from">>),

        %% Karen and Carol cannot sent to each other now
        escalus_client:send(Karen, escalus_stanza:chat_to(
                                   Carol, <<"Hi, Carol XYZ!">>)),
        escalus_client:send(Carol, escalus_stanza:chat_to(
                                     Karen, <<"Hi, Karen XYZ!">>)),

        ct:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Carol),
        escalus_assert:has_no_stanzas(Karen),

        %% Carol subscribes to Karen
        escalus_client:send(Carol,
                            escalus_stanza:presence_direct(
                              karen, <<"subscribe">>)),
        escalus_client:wait_for_stanza(Carol),
        escalus_client:wait_for_stanza(Karen),

        %% Karen accepts Carol
        escalus_client:send(Karen, escalus_stanza:presence_direct(
                                   carol, <<"subscribed">>)),
        escalus_client:wait_for_stanza(Karen),
        escalus_client:wait_for_stanzas(Carol, 3),

        %% Now Carol is subscirbed "to" Karen
        %% And Karen is subscribed "from" Carol

        escalus_client:send(Karen, escalus_stanza:chat_to(
                                   Carol, <<"Hi, Carol XYZ!">>)),
        escalus_assert:is_chat_message(<<"Hi, Carol XYZ!">>,
            escalus_client:wait_for_stanza(Carol)),

        escalus_client:send(Carol, escalus_stanza:chat_to(
                                     Karen, <<"Hi, Karen XYZ!">>)),
        escalus_assert:is_chat_message(<<"Hi, Karen XYZ!">>,
            escalus_client:wait_for_stanza(Karen))

    end).


allow_subscription_both_message(Config) ->
    escalus:story(Config, [{carol, 1}], fun(Carol) ->

        [{_, Spec}] = escalus_users:get_users([karen]),
        {ok, Karen, _Spec2, _Features} = escalus_connection:start(Spec),
        %escalus_story:send_initial_presence(Carol),
        escalus_story:send_initial_presence(Karen),
        escalus_client:wait_for_stanza(Carol),
        escalus_client:wait_for_stanza(Karen),
        %% deny all message but not from subscribed "to"
        privacy_helper:set_and_activate(
          Carol, <<"deny_all_message_but_subscription_both">>),

        %% deny all message but not from subscribed "from"
        privacy_helper:set_and_activate(
          Karen, <<"deny_all_message_but_subscription_both">>),

        %% Karen and Carol cannot sent to each other now
        %% Even though they are in subscription "to" and "from" respectively
        escalus_client:send(
          Karen, escalus_stanza:chat_to(Carol, <<"Hi, Carol XYZ!">>)),
        escalus_client:send(
          Carol, escalus_stanza:chat_to(karen, <<"Hi, Karen XYZ!">>)),

        ct:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Carol),
        escalus_assert:has_no_stanzas(Karen),

        %% Carol subscribes to Karen
        escalus_client:send(Karen,
                            escalus_stanza:presence_direct(
                              carol, <<"subscribe">>)),
        escalus_client:wait_for_stanza(Carol),
        escalus_client:wait_for_stanza(Karen),

        %% Karen accepts Carol
        escalus_client:send(Carol, escalus_stanza:presence_direct(
                                     karen, <<"subscribed">>)),
        escalus_client:wait_for_stanzas(Carol, 2),
        escalus_client:wait_for_stanzas(Karen, 3),

        %% Now their subscription is in state "both"
        escalus_client:send(Karen, escalus_stanza:chat_to(
                                   Carol, <<"Hi, Carol XYZ!">>)),
        escalus_assert:is_chat_message(<<"Hi, Carol XYZ!">>,
            escalus_client:wait_for_stanza(Carol)),

        escalus_client:send(Carol, escalus_stanza:chat_to(
                                     karen, <<"Hi, Karen XYZ!">>)),
        escalus_assert:is_chat_message(<<"Hi, Karen XYZ!">>,
            escalus_client:wait_for_stanza(Karen))

    end).

block_all_message(Config) ->
    escalus:story(Config, [{carol, 1}, {karen, 1}], fun(Carol, Karen) ->

        %% Carol should receive message
        escalus_client:send(Karen, escalus_stanza:chat_to(Carol, <<"Hi!">>)),
        escalus_assert:is_chat_message(<<"Hi!">>,
            escalus_client:wait_for_stanza(Carol)),

        %% set the list on server and make it active
        privacy_helper:set_and_activate(Carol, <<"deny_all_message">>),

        %% Carol should NOT receive message
        escalus_client:send(Karen, escalus_stanza:chat_to(Carol, <<"Hi!">>)),
        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Carol)

        end).

block_jid_presence_in(Config) ->
    escalus:story(Config, [{carol, 1}, {karen, 1}], fun(Carol, Karen) ->

        %% Carol should receive presence in
        escalus_client:send(Karen,
            escalus_stanza:presence_direct(carol, <<"available">>)),
        Received = escalus_client:wait_for_stanza(Carol),
        escalus:assert(is_presence, Received),
        escalus_assert:is_stanza_from(Karen, Received),

        privacy_helper:set_and_activate(Carol, <<"deny_karen_presence_in">>),

        %% Carol should NOT receive presence in
        escalus_client:send(Karen,
            escalus_stanza:presence_direct(carol, <<"available">>)),
        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Carol)

        end).

block_jid_presence_out(Config) ->
    escalus:story(Config, [{carol, 1}, {karen, 1}], fun(Carol, Karen) ->

        %% Karen should receive presence in
        escalus_client:send(Carol,
            escalus_stanza:presence_direct(karen, <<"available">>)),
        Received = escalus_client:wait_for_stanza(Karen),
        escalus:assert(is_presence, Received),
        escalus_assert:is_stanza_from(Carol, Received),

        privacy_helper:set_and_activate(Carol, <<"deny_karen_presence_out">>),

        %% Karen should NOT receive presence in
        escalus_client:send(Carol,
            escalus_stanza:presence_direct(karen, <<"available">>)),

        %% Carol gets an error back from mod_privacy
        Presence = escalus_client:wait_for_stanza(Carol),
        escalus:assert(is_presence_with_type, [<<"error">>], Presence),

        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Karen)

        end).

block_jid_iq(Config) ->
    escalus:story(Config, [{carol, 1}, {karen, 1}], fun(Carol, _Karen) ->

        privacy_helper:set_list(Carol, <<"deny_localhost_iq">>),
        %% activate it
        escalus_client:send(Carol,
            escalus_stanza:privacy_activate(<<"deny_localhost_iq">>)),
        %% From now on no iq replies should reach Carol.
        %% That's also the reason why we couldn't use
        %% the privacy_helper:set_and_activate helper -
        %% it waits for all replies.

        %% Just set the toy list and ensure that only
        %% the notification push comes back.
        privacy_helper:send_set_list(Carol, <<"deny_karen">>),
        Response = escalus_client:wait_for_stanza(Carol),
        escalus:assert(fun privacy_helper:is_privacy_list_push/1, Response),
        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Carol)

        end).

block_jid_all(Config) ->
    escalus:story(Config, [{carol, 1}, {karen, 1}], fun(Carol, Karen) ->

        privacy_helper:set_list(Carol, <<"deny_jid_all">>),

        %% Carol blocks Karen
        escalus_client:send(Carol,
            escalus_stanza:privacy_activate(<<"deny_jid_all">>)),
        %% IQ response is blocked;
        %% do magic wait for the request to take effect
        timer:sleep(200),

        %% From now on nothing whatsoever sent by Karen should reach Carol.

        %% Carol should NOT receive message
        escalus_client:send(Karen, escalus_stanza:chat_to(Carol, <<"Hi!">>)),

        %% Carol should NOT receive presence-in from Karen
        escalus_client:send(Karen,
            escalus_stanza:presence_direct(carol, <<"available">>)),

        %% Karen should NOT receive presence-in from Carol
        escalus_client:send(Carol,
            escalus_stanza:presence_direct(karen, <<"available">>)),

        %% Just set the toy list and ensure that only
        %% the notification push comes back.
        privacy_helper:send_set_list(Carol, <<"deny_karen">>),

        %% verify
        timer:sleep(?SLEEP_TIME),
        %% ...that nothing reached Karen
        escalus_assert:has_no_stanzas(Karen),
        %% ...that Carol got exactly two responses
        Responses = escalus_client:wait_for_stanzas(Carol, 2),
        %% one of which is a push and the other a presence error
        escalus:assert_many([
            fun privacy_helper:is_privacy_list_push/1,
            fun privacy_helper:is_presence_error/1
        ], Responses),
        %% and Carol didn't get anything else
        escalus_assert:has_no_stanzas(Carol)

        end).

block_jid_message_but_not_presence(Config) ->
    escalus:story(Config, [{carol, 1}, {karen, 1}], fun(Carol, Karen) ->

        %% Carol should receive message
        escalus_client:send(Karen,
            escalus_stanza:chat_to(Carol, <<"Hi! What's your name?">>)),
        escalus_assert:is_chat_message(<<"Hi! What's your name?">>,
            escalus_client:wait_for_stanza(Carol)),

        %% set the list on server and make it active
        privacy_helper:set_and_activate(Carol, <<"deny_karen_message">>),

        %% Carol should NOT receive message
        escalus_client:send(Karen, escalus_stanza:chat_to(
                                   Carol, <<"Hi, Carol!">>)),
        timer:sleep(?SLEEP_TIME),
        escalus_assert:has_no_stanzas(Carol),

        %% ...but should receive presence in
        escalus_client:send(Karen,
            escalus_stanza:presence_direct(Carol, <<"available">>)),
        Received = escalus_client:wait_for_stanza(Carol),
        escalus:assert(is_presence, Received),
        escalus_assert:is_stanza_from(Karen, Received)

        end).

%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------

add_sample_contact(Who, Whom, Groups, Nick) ->
    escalus_client:send(Who,
                        escalus_stanza:roster_add_contact(Whom,
                                                          Groups,
                                                          Nick)),
    Received = escalus_client:wait_for_stanza(Who),
    escalus_assert:is_roster_set(Received),
    escalus_client:send(Who, escalus_stanza:iq_result(Received)),
    escalus:assert(is_iq_result, escalus:wait_for_stanza(Who)).
