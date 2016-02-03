%%==============================================================================
%% Copyright 2013 Erlang Solutions Ltd.
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

-module(ejabberd_roster_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").


%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, roster},
     {group, roster_versioning},
     {group, subscribe_group}].

groups() ->
    [{roster, [sequence], [get_roster,
                           add_contact,
                           roster_push,
                           remove_contact]},
     {roster_versioning, [sequence], [versioning]},
     {subscribe_group, [sequence], [subscribe,
                                    subscribe_decline,
                                    subscribe_relog,
                                    unsubscribe,
                                    remove_unsubscribe]}].

suite() ->
    [{required, ejabberd_node} | escalus:suite()].


%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------


init_per_suite(Config) ->
    net_kernel:start(['mongooseim@localhost', longnames]),

    wocky_app:start(),

    application:load(ejabberd),
    DataDir = proplists:get_value(data_dir, Config),
    ConfigPath = filename:join([DataDir, "ejabberd.cfg"]),
    application:set_env(ejabberd, config, ConfigPath),
    application:ensure_all_started(ejabberd),

    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config),
    application:stop(ejabberd),
    wocky_app:stop(),
    ok.

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config, {by_name, [alice, bob]}).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, {by_name, [alice, bob]}).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config)
  when CaseName == add_contact; CaseName == roster_push ->
    [{_, UserSpec} | _] = escalus_config:get_config(escalus_users, Config),
    remove_roster(Config, UserSpec),
    escalus:end_per_testcase(CaseName, Config);
end_per_testcase(CaseName, Config)
  when CaseName == subscribe; CaseName == subscribe_decline;
       CaseName == unsubscribe; CaseName == versioning ->
    [{_, UserSpec1}, {_, UserSpec2} | _] =
        escalus_config:get_config(escalus_users, Config),
    remove_roster(Config, UserSpec1),
    remove_roster(Config, UserSpec2),
    escalus:end_per_testcase(CaseName, Config);
end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).


%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

get_roster(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun (Alice, _Bob) ->
        escalus_client:send(Alice, escalus_stanza:roster_get()),
        escalus_assert:is_roster_result(escalus:wait_for_stanza(Alice))
    end).

add_contact(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun (Alice, Bob) ->
        %% add contact
        Stanza = escalus_stanza:roster_add_contact(Bob, [<<"friends">>],
                                                   <<"Bobby">>),
        escalus:send(Alice, Stanza),
        Received = escalus:wait_for_stanzas(Alice, 2),
        escalus:assert_many([is_roster_set, is_iq_result], Received),

        Result = hd([R || R <- Received, escalus_pred:is_roster_set(R)]),
        escalus:assert(count_roster_items, [1], Result),
        escalus:send(Alice, escalus_stanza:iq_result(Result)),

        %% check roster
        escalus:send(Alice, escalus_stanza:roster_get()),
        Received2 = escalus:wait_for_stanza(Alice),

        escalus:assert(is_roster_result, Received2),
        escalus:assert(roster_contains, [bob], Received2)
    end).

roster_push(Config) ->
    escalus:story(Config, [{alice, 2}, {bob, 1}], fun (Alice1, Alice2, Bob) ->
        %% add contact
        Stanza = escalus_stanza:roster_add_contact(Bob, [<<"friends">>],
                                                   <<"Bobby">>),
        escalus:send(Alice1, Stanza),
        Received = escalus_client:wait_for_stanza(Alice1),
        escalus_client:send(Alice1, escalus_stanza:iq_result(Received)),
        escalus_client:wait_for_stanza(Alice1),

        Received2 = escalus_client:wait_for_stanza(Alice2),
        escalus_client:send(Alice2, escalus_stanza:iq_result(Received2))
    end).

remove_contact(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun (Alice, Bob) ->
        %% add contact
        add_sample_contact(Alice, Bob),

        %% check roster
        escalus:send(Alice, escalus_stanza:roster_get()),
        escalus:assert(count_roster_items, [1], escalus:wait_for_stanza(Alice)),

        %% remove contact
        escalus:send(Alice, escalus_stanza:roster_remove_contact(Bob)),
        escalus:assert_many([is_roster_set, is_iq_result],
                            escalus:wait_for_stanzas(Alice, 2)),

        %% check roster
        escalus:send(Alice, escalus_stanza:roster_get()),
        escalus:assert(count_roster_items, [0], escalus:wait_for_stanza(Alice))
    end).

versioning(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        escalus:send(Alice, escalus_stanza:roster_get(<<"">>)),
        RosterResult = escalus:wait_for_stanza(Alice),

        escalus_assert:is_roster_result(RosterResult),
        Ver = exml_query:path(RosterResult, [{element, <<"query">>},
                                             {attr, <<"ver">>}]),

        true = Ver /= undefined,

        %% add contact
        Stanza = escalus_stanza:roster_add_contact(Bob, [<<"friends">>],
                                                   <<"Bobby">>),
        escalus:send(Alice, Stanza),
        Received = escalus:wait_for_stanzas(Alice, 2),

        escalus:assert_many([is_roster_set, is_iq_result], Received),

        RosterSet = hd(Received),

        Ver2 = exml_query:path(RosterSet, [{element, <<"query">>},
                                           {attr, <<"ver">>}]),

        true = Ver2 /= undefined,

        Result = hd([R || R <- Received, escalus_pred:is_roster_set(R)]),
        escalus:assert(count_roster_items, [1], Result),
        escalus:send(Alice, escalus_stanza:iq_result(Result)),

        %% check roster, send old ver
        escalus:send(Alice, escalus_stanza:roster_get(Ver)),
        Received2 = escalus:wait_for_stanza(Alice),

        escalus:assert(is_roster_result, Received2),
        escalus:assert(roster_contains, [bob], Received2),

        %% check version
        Ver2 = exml_query:path(Received2, [{element, <<"query">>},
                                           {attr, <<"ver">>}]),

        %% check roster, send correct Ver
        escalus:send(Alice, escalus_stanza:roster_get(Ver2)),
        Received3 = escalus:wait_for_stanza(Alice),

        escalus:assert(is_iq_result, Received3),

        %% There are no items as version matches
        undefined = exml_query:path(Received3, [{element, <<"item">>}])
    end).

subscribe(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun (Alice, Bob) ->
        %% Alice adds Bob as a contact
        add_sample_contact(Alice, Bob),

        %% She subscribes to his presences
        escalus:send(Alice, escalus_stanza:presence_direct(bob,
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
        escalus:send(Bob, escalus_stanza:presence_direct(alice,
                                                         <<"subscribed">>)),

        %% Alice receives subscribed
        Stanzas = escalus:wait_for_stanzas(Alice, 2),

        check_subscription_stanzas(Stanzas, <<"subscribed">>),
        escalus:assert(is_presence, escalus:wait_for_stanza(Alice)),

        %% Bob receives roster push
        PushReqB1 = escalus:wait_for_stanza(Bob),
        escalus:assert(is_roster_set, PushReqB1),

        %% Bob sends presence
        escalus:send(Bob, escalus_stanza:presence(<<"available">>)),
        escalus:assert(is_presence, escalus:wait_for_stanza(Alice)),

        %% Bob sends presence
        escalus:send(Bob, escalus_stanza:presence(<<"unavailable">>)),
        escalus:assert(is_presence_with_type, [<<"unavailable">>],
                       escalus:wait_for_stanza(Alice))
    end).

subscribe_decline(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun (Alice, Bob) ->
        %% add contact
        add_sample_contact(Alice, Bob),

        %% subscribe
        escalus:send(Alice, escalus_stanza:presence_direct(bob,
                                                           <<"subscribe">>)),
        PushReq = escalus:wait_for_stanza(Alice),
        escalus_assert:is_roster_set(PushReq),
        escalus:send(Alice, escalus_stanza:iq_result(PushReq)),

        %% Bob receives subscription reqest
        Received = escalus:wait_for_stanza(Bob),
        escalus:assert(is_presence_with_type, [<<"subscribe">>], Received),

        %% Bob refuses subscription
        escalus:send(Bob, escalus_stanza:presence_direct(alice,
                                                         <<"unsubscribed">>)),

        %% Alice receives subscribed
        Stanzas = escalus:wait_for_stanzas(Alice, 2),

        check_subscription_stanzas(Stanzas, <<"unsubscribed">>)
    end).


subscribe_relog(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        %% Alice adds Bob as a contact
        add_sample_contact(Alice, Bob),

        %% She subscribes to his presences
        escalus:send(Alice,
                     escalus_stanza:presence_direct(bob, <<"subscribe">>)),

        PushReq = escalus:wait_for_stanza(Alice),
        escalus:assert(is_roster_set, PushReq),
        escalus:send(Alice, escalus_stanza:iq_result(PushReq)),

        %% Bob receives subscription reqest
        Received = escalus:wait_for_stanza(Bob),
        escalus:assert(is_presence_with_type, [<<"subscribe">>], Received),

        %% New Bob resource connects, should receive subscription request again
        {ok, NewBob} = escalus_client:start_for(Config, bob, <<"newbob">>),
        escalus:send(NewBob,
                     escalus_stanza:presence(<<"available">>)),

        escalus:assert(is_presence_with_type, [<<"available">>],
                       escalus:wait_for_stanza(Bob)),

        Stanzas = escalus:wait_for_stanzas(NewBob, 3),
        3 = length(Stanzas),

        escalus_new_assert:mix_match([
                fun (S) ->
                    escalus_pred:is_presence_with_type(<<"available">>, S)
                    andalso escalus_pred:is_stanza_from(Bob, S)
                end,
                fun (S) ->
                    escalus_pred:is_presence_with_type(<<"available">>, S)
                    andalso escalus_pred:is_stanza_from(NewBob, S)
                end,
                fun (S) ->
                    escalus_pred:is_presence_with_type(<<"subscribe">>, S)
                    andalso escalus_pred:is_stanza_from(alice, S)
                end
            ], Stanzas),

        escalus_client:stop(NewBob),

        escalus:send(Bob,
                     escalus_stanza:presence_direct(alice, <<"unsubscribed">>))
    end).

unsubscribe(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun (Alice, Bob) ->
        %% add contact
        add_sample_contact(Alice, Bob),

        %% subscribe
        escalus:send(Alice,
                     escalus_stanza:presence_direct(bob, <<"subscribe">>)),
        PushReq = escalus:wait_for_stanza(Alice),
        escalus:send(Alice, escalus_stanza:iq_result(PushReq)),

        %% Bob receives subscription reqest
        escalus:assert(is_presence_with_type, [<<"subscribe">>],
                       escalus:wait_for_stanza(Bob)),
        %% Bob adds new contact to his roster
        escalus:send(Bob, escalus_stanza:roster_add_contact(Alice,
                                                            [<<"enemies">>],
                                                             <<"Alice">>)),
        PushReqB = escalus:wait_for_stanza(Bob),
        escalus:send(Bob, escalus_stanza:iq_result(PushReqB)),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),

        %% Bob sends subscribed presence
        escalus:send(Bob,
                     escalus_stanza:presence_direct(alice, <<"subscribed">>)),

        %% Alice receives subscribed
        Stanzas = escalus:wait_for_stanzas(Alice, 2),

        check_subscription_stanzas(Stanzas, <<"subscribed">>),
        escalus:assert(is_presence, escalus:wait_for_stanza(Alice)),

        %% Bob receives roster push
        PushReqB1 = escalus:wait_for_stanza(Bob),
        escalus_assert:is_roster_set(PushReqB1),

        %% Alice sends unsubscribe
        escalus:send(Alice,
                     escalus_stanza:presence_direct(bob, <<"unsubscribe">>)),

        PushReqA2 = escalus:wait_for_stanza(Alice),
        escalus_assert:is_roster_set(PushReqA2),
        escalus:send(Alice, escalus_stanza:iq_result(PushReqA2)),

        %% Bob receives unsubscribe

        StanzasB = escalus:wait_for_stanzas(Bob, 2),

        check_subscription_stanzas(StanzasB, <<"unsubscribe">>),

        %% Alice receives unsubscribed
        escalus:assert(is_presence_with_type, [<<"unavailable">>],
                       escalus:wait_for_stanza(Alice))
    end).

remove_unsubscribe(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun (Alice, Bob) ->
        %% add contact
        add_sample_contact(Alice, Bob),

        %% subscribe
        escalus:send(Alice,
                     escalus_stanza:presence_direct(bob, <<"subscribe">>)),
        PushReq = escalus:wait_for_stanza(Alice),
        escalus:send(Alice, escalus_stanza:iq_result(PushReq)),

        %% Bob receives subscription reqest
        escalus:assert(is_presence_with_type, [<<"subscribe">>],
                       escalus:wait_for_stanza(Bob)),

        %% Bob adds new contact to his roster
        escalus:send(Bob, escalus_stanza:roster_add_contact(Alice,
                                                            [<<"enemies">>],
                                                            <<"Alice">>)),
        PushReqB = escalus:wait_for_stanza(Bob),
        escalus:send(Bob, escalus_stanza:iq_result(PushReqB)),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),

        %% Bob sends subscribed presence
        escalus:send(Bob, escalus_stanza:presence_direct(alice,
                                                         <<"subscribed">>)),

        %% Alice receives subscribed
        Stanzas = [escalus:wait_for_stanza(Alice),
                   escalus:wait_for_stanza(Alice)],

        check_subscription_stanzas(Stanzas, <<"subscribed">>),
        escalus:assert(is_presence, escalus:wait_for_stanza(Alice)),

        %% Bob receives roster push
        PushReqB1 = escalus:wait_for_stanza(Bob),
        escalus_assert:is_roster_set(PushReqB1),

        %% remove contact
        escalus:send(Alice, escalus_stanza:roster_remove_contact(Bob)),

        IsPresUnavailable =
                fun (S) ->
                    escalus_pred:is_presence_with_type(<<"unavailable">>, S)
                end,
        escalus:assert_many([is_roster_set, is_iq_result, IsPresUnavailable],
                            escalus:wait_for_stanzas(Alice, 3)),
        check_subscription_stanzas(escalus:wait_for_stanzas(Bob, 2),
                                   <<"unsubscribe">>)
    end).


%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------

add_sample_contact(Alice, Bob) ->
    escalus:send(Alice,
                 escalus_stanza:roster_add_contact(Bob, [<<"friends">>],
                                                   <<"Bobby">>)),

    Received = escalus:wait_for_stanzas(Alice, 2),
    escalus:assert_many([is_roster_set, is_iq_result], Received),

    Result = hd([R || R <- Received, escalus_pred:is_roster_set(R)]),
    escalus:assert(count_roster_items, [1], Result),
    escalus:send(Alice, escalus_stanza:iq_result(Result)).

check_subscription_stanzas(Stanzas, Type) ->
    IsPresWithType = fun (S) ->
                         escalus_pred:is_presence_with_type(Type, S)
                     end,
    escalus:assert_many([is_roster_set, IsPresWithType], Stanzas).

remove_roster(Config, UserSpec) ->
    [User, Server, _] = [escalus_ejabberd:unify_str_arg(Item) ||
                            Item <- escalus_users:get_usp(Config, UserSpec)],

    ok = escalus_ejabberd:rpc(mod_wocky_roster, remove_user_hook,
                              [User, Server]).
