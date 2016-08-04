%%% @copyright 2016+ Hippware, Inc.
%%% @doc Helper functions for Wocky integration test suites
-module(test_helper).

-include("wocky_db_seed.hrl").

-export([ensure_wocky_is_running/0]).

-export([expect_iq_success/2,
         expect_iq_error/2,
         expect_iq_success_u/2,
         expect_iq_error_u/2,

         expect_friendship_presence/2,
         subscribe/2,
         check_subscription_stanzas/2,
         add_sample_contact/2]).

ensure_wocky_is_running() ->
    case net_kernel:start(['mongooseim@localhost', longnames]) of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok
    end,

    ok = wocky_app:start("ct").

expect_iq_success_u(Stanza, User) ->
    expect_something(add_to_u(Stanza, User), User, is_iq_result).

expect_iq_error_u(Stanza, User) ->
    expect_something(add_to_s(Stanza, User), User, is_iq_error).

add_to_u(Stanza, User) ->
    escalus_stanza:to(Stanza,
      escalus_client:short_jid(User)).

expect_iq_success(Stanza, User) ->
    expect_something(add_to_s(Stanza, User), User, is_iq_result).

expect_iq_error(Stanza, User) ->
    expect_something(add_to_s(Stanza, User), User, is_iq_error).

expect_something(Stanza, User, Expect) ->
    ct:log("Sending stanza: ~p", [Stanza]),
    ResultStanza = escalus:send_and_wait(User, Stanza),
    ct:log("Result stanza: ~p", [ResultStanza]),
    escalus:assert(Expect, ResultStanza),
    ResultStanza.

add_to_s(Stanza, User) ->
    escalus_stanza:to(Stanza,
      escalus_client:server(User)).


expect_friendship_presence(User1, User2) ->
    lists:foreach(fun(U) ->
                          [S1, S2] = escalus:wait_for_stanzas(U, 2),
                          escalus:assert(is_presence_stanza, S1),
                          escalus:assert(is_presence_stanza, S2)
                  end,
                  [User1, User2]).

subscribe(Alice, Bob) ->
    %% Alice adds Bob as a contact
    add_sample_contact(Alice, Bob),

    %% She subscribes to his presences
    escalus:send(Alice, escalus_stanza:presence_direct(?BOB_B_JID,
                                                       <<"subscribe">>)),
    PushReq = escalus:wait_for_stanza(Alice),
    escalus:assert(is_roster_set, PushReq),
    escalus:send(Alice, escalus_stanza:iq_result(PushReq)),

    %% Bob receives subscription reqest
    Received = escalus:wait_for_stanza(Bob),
    escalus:assert(is_presence_with_type, [<<"subscribe">>], Received),

    %% Bob adds new contact to his roster
    escalus:send(Bob, escalus_stanza:roster_add_contact(Alice,
                                                        [<<"enemies">>],
                                                        <<"Alice">>)),
    PushReqB = escalus:wait_for_stanza(Bob),
    escalus:assert(is_roster_set, PushReqB),
    escalus:send(Bob, escalus_stanza:iq_result(PushReqB)),
    escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),

    %% Bob sends subscribed presence
    escalus:send(Bob, escalus_stanza:presence_direct(?ALICE_B_JID,
                                                     <<"subscribed">>)),

    %% Alice receives subscribed
    Stanzas = escalus:wait_for_stanzas(Alice, 2),

    check_subscription_stanzas(Stanzas, <<"subscribed">>),
    escalus:assert(is_presence, escalus:wait_for_stanza(Alice)),

    %% Bob receives roster push
    PushReqB1 = escalus:wait_for_stanza(Bob),
    escalus:assert(is_roster_set, PushReqB1).

check_subscription_stanzas(Stanzas, Type) ->
    IsPresWithType = fun (S) ->
                         escalus_pred:is_presence_with_type(Type, S)
                     end,
    escalus:assert_many([is_roster_set, IsPresWithType], Stanzas).

add_sample_contact(Alice, Bob) ->
    escalus:send(Alice,
                 escalus_stanza:roster_add_contact(Bob, [<<"friends">>],
                                                   <<"Bobby">>)),

    Received = escalus:wait_for_stanzas(Alice, 2),
    escalus:assert_many([is_roster_set, is_iq_result], Received),

    Result = hd([R || R <- Received, escalus_pred:is_roster_set(R)]),
    escalus:assert(count_roster_items, [1], Result),
    escalus:send(Alice, escalus_stanza:iq_result(Result)).
