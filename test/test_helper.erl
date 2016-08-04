%%% @copyright 2016+ Hippware, Inc.
%%% @doc Helper functions for Wocky integration test suites
-module(test_helper).

-include("wocky_db_seed.hrl").

-export([ensure_wocky_is_running/0,
         make_everyone_friends/2
        ]).

-export([expect_iq_success/2,
         expect_iq_error/2,
         expect_iq_success_u/2,
         expect_iq_error_u/2,

         ensure_all_clean/1,

         expect_friendship_presence/2,
         subscribe/2,
         expect_subscription_stanzas/2,
         subscribe_pair/2,
         add_contact/4
        ]).

ensure_wocky_is_running() ->
    case net_kernel:start(['mongooseim@localhost', longnames]) of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok
    end,

    ok = wocky_app:start("ct").

ensure_all_clean(Clients) ->
    lists:foreach(fun(Client) ->
        escalus_assert:has_no_stanzas(Client)
    end, Clients).

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

add_contact(Who, Whom, Groups, Nick) ->
    escalus_client:send(Who,
                        escalus_stanza:roster_add_contact(Whom,
                                                          Groups,
                                                          Nick)),
    Received = escalus_client:wait_for_stanza(Who),
    escalus_assert:is_roster_set(Received),
    reply_to_roster_set(Who, [Received]),
    escalus:assert(is_iq_result, escalus:wait_for_stanza(Who)).

subscribe(Who, Whom) ->
    %% 'Who' sends a subscribe request to 'Whom'
    escalus:send(Who, escalus_stanza:presence_direct(
                        escalus_client:short_jid(Whom), <<"subscribe">>)),
    PushReq = escalus:wait_for_stanza(Who),
    escalus:assert(is_roster_set, PushReq),
    reply_to_roster_set(Who, [PushReq]),

    %% 'Whom' receives subscription reqest
    %% In wocky contact is auto-accepted and added to roster
    Stanzas = expect_subscription_stanzas(Whom, <<"subscribe">>),
    reply_to_roster_set(Whom, Stanzas),

    %% 'Who' receives subscribed
    expect_subscription_stanzas(Who, <<"subscribed">>).

expect_subscription_stanzas(Who, Type) ->
    Stanzas = escalus:wait_for_stanzas(Who, 2),
    IsPresWithType = fun (S) ->
                         escalus_pred:is_presence_with_type(Type, S)
                     end,
    escalus:assert_many([is_roster_set, IsPresWithType], Stanzas),
    Stanzas.

reply_to_roster_set(Client, Stanzas) when is_list(Stanzas) ->
    RosterSet = hd([R || R <- Stanzas, escalus_pred:is_roster_set(R)]),
    reply_to_roster_set(Client, RosterSet);

reply_to_roster_set(Client, RosterSet) ->
    escalus:send(Client, escalus_stanza:iq_result(RosterSet)),
    escalus:assert(count_roster_items, [1], RosterSet).

make_everyone_friends(Config0, Users) ->
    % start the clients
    Config1 = escalus_cleaner:start(Config0),
    Clients = start_clients_before_all_friends(
                Config1, [[{US, <<"friendly">>}] || {_Name, US} <- Users]),

    % exchange subscribe and subscribed stanzas
    escalus_utils:distinct_pairs(fun subscribe_pair/2, Clients),

    ensure_all_clean(Clients),

    % stop the clients
    escalus_cleaner:clean(Config1),
    escalus_cleaner:stop(Config1),

    % return Config0
    [{everyone_is_friends, true} | Config0].

subscribe_pair(Alice, Bob) ->
    subscribe(Alice, Bob),
    subscribe(Bob, Alice),
    Presence = escalus:wait_for_stanza(Bob),
    escalus:assert(is_presence, Presence).

start_clients_before_all_friends(Config, ClientDescs) ->
    ct:log("start_clients_all_friends ~p", [ClientDescs]),
    lists:flatmap(fun(UserCDs) ->
                          call_start_ready_clients(Config, UserCDs)
                  end, ClientDescs).

call_start_ready_clients(Config, UserCDs) ->
    escalus_overridables:do(Config, start_ready_clients, [Config, UserCDs],
                            {escalus_story, start_ready_clients}).
