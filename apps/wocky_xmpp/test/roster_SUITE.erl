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
-module(roster_SUITE).

-compile(export_all).
-compile({parse_transform, fun_chain}).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("test_helper.hrl").
-include("wocky.hrl").

-define(BOB_HANDLE, <<"Bob">>).
-define(AVATAR_ID, <<"ba191e16-8c87-11e7-8372-5f4e6ef88769">>).
-define(BOB_AVATAR, ?tros:make_url(?SERVER, ?AVATAR_ID)).
-define(BOB_FIRST_NAME, <<"Bobble">>).
-define(BOB_LAST_NAME, <<"Robertson">>).
-define(ROLE1, <<"a role">>).
-define(ROLE2, <<"a different role">>).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, roster},
     {group, roster_versioning},
     {group, subscribe_group}].

groups() ->
  [{roster, [], [get_roster,
                 add_contact,
                 roster_push,
                 remove_contact]},
   {roster_versioning, [], [versioning]},
   {subscribe_group, [], [subscribe,
                          unsubscribe,
                          remove_unsubscribe
                         ]}].

suite() ->
    escalus:suite().


%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),
    Config2 = fun_chain:first(Config,
        escalus:init_per_suite(),
        test_helper:setup_users([alice, bob, carol])
    ),

    Bob = ?wocky_repo:get(?wocky_user, ?BOB),
    ?wocky_factory:insert(tros_metadata, [{id, ?AVATAR_ID}, {user, Bob}]),

    ok = ?wocky_user:update(?BOB, #{handle => ?BOB_HANDLE,
                                    avatar => ?BOB_AVATAR,
                                    roles => [?ROLE1, ?ROLE2],
                                    first_name => ?BOB_FIRST_NAME,
                                    last_name => ?BOB_LAST_NAME}),

    Config2.

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config)
  when CaseName =:= add_contact; CaseName =:= roster_push ->
    [{_, UserSpec} | _] = escalus_config:get_config(escalus_users, Config),
    remove_roster(Config, UserSpec),
    escalus:end_per_testcase(CaseName, Config);
end_per_testcase(CaseName, Config)
  when CaseName =:= subscribe;
       CaseName =:= unsubscribe; CaseName =:= versioning ->
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
        check_extra_fields(Result),
        escalus:send(Alice, escalus_stanza:iq_result(Result)),

        %% check roster
        escalus:send(Alice, escalus_stanza:roster_get()),
        Received2 = escalus:wait_for_stanza(Alice),

        escalus:assert(is_roster_result, Received2),
        escalus:assert(roster_contains, [?BJID(?BOB)], Received2),
        check_extra_fields(Received2),
        ?assert(has_created_at(Received2))
    end).

roster_push(Config) ->
    escalus:story(Config, [{alice, 2}, {bob, 1}], fun (Alice1, Alice2, Bob) ->
        %% add contact
        Stanza = escalus_stanza:roster_add_contact(Bob, [<<"friends">>],
                                                   <<"Bobby">>),
        escalus:send(Alice1, Stanza),
        Received = escalus_client:wait_for_stanza(Alice1),
        check_extra_fields(Received),
        escalus_client:send(Alice1, escalus_stanza:iq_result(Received)),
        escalus_client:wait_for_stanza(Alice1),

        Received2 = escalus_client:wait_for_stanza(Alice2),
        check_extra_fields(Received2),
        escalus_client:send(Alice2, escalus_stanza:iq_result(Received2))
    end).

remove_contact(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun (Alice, Bob) ->
        %% add contact
        test_helper:add_contact(Alice, Bob, [<<"friends">>], <<"Bobsicle">>),

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
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}],
                  fun(Alice, Bob, Carol) ->
        escalus:send(Alice, escalus_stanza:roster_get(<<"">>)),
        RosterResult = escalus:wait_for_stanza(Alice),

        escalus_assert:is_roster_result(RosterResult),
        Ver = get_ver(RosterResult),

        true = Ver =:= undefined,

        %% add contact
        Stanza = escalus_stanza:roster_add_contact(Bob, [<<"friends">>],
                                                   <<"Bobby">>),
        escalus:send(Alice, Stanza),
        Received = escalus:wait_for_stanzas(Alice, 2),

        escalus:assert_many([is_roster_set, is_iq_result], Received),

        RosterSet = hd(Received),

        Ver2 = get_ver(RosterSet),

        true = Ver2 =/= undefined,

        Result = hd([R || R <- Received, escalus_pred:is_roster_set(R)]),
        escalus:assert(count_roster_items, [1], Result),
        check_extra_fields(Result),
        escalus:send(Alice, escalus_stanza:iq_result(Result)),

        %% check roster, no old ver
        escalus:send(Alice, escalus_stanza:roster_get()),
        Received2 = escalus:wait_for_stanza(Alice),

        escalus:assert(is_roster_result, Received2),
        escalus:assert(roster_contains, [?BJID(?BOB)], Received2),
        ?assert(has_created_at(Received2)),

        %% check version
        Ver2 = get_ver(Received2),

        %% check roster, send correct Ver
        escalus:send(Alice, escalus_stanza:roster_get(Ver2)),
        Received3 = escalus:wait_for_stanza(Alice),

        escalus:assert(is_iq_result, Received3),

        %% There are no items as version matches
        undefined = exml_query:path(Received3, [{element, <<"query">>},
                                                {element, <<"item">>}]),

        %% add another contact
        Stanza2 = escalus_stanza:roster_add_contact(Carol, [<<"friends">>],
                                                    <<"Cazza">>),
        escalus:send(Alice, Stanza2),
        Received4 = escalus:wait_for_stanzas(Alice, 2),

        escalus:assert_many([is_roster_set, is_iq_result], Received4),

        RosterSet2 = hd(Received4),

        Ver3 = get_ver(RosterSet2),

        true = Ver3 =/= undefined,

        Result2 = hd([R || R <- Received4, escalus_pred:is_roster_set(R)]),
        escalus:assert(count_roster_items, [1], Result2),
        escalus:send(Alice, escalus_stanza:iq_result(Result2)),

        % Get the roster update, based on an older version:
        escalus:send(Alice, escalus_stanza:roster_get(Ver2)),
        Received5 = escalus:wait_for_stanza(Alice),

        escalus:assert(is_iq_result, Received5),

        % We asked for an old version, so we should get an update with
        % both records in it.
        2 = length(exml_query:paths(Received5, [{element, <<"query">>},
                                                {element, <<"item">>}]))
    end).

subscribe(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun (Alice, Bob) ->
        %% Only bi-directional subscriptions send presence data by default in
        %% wocky
        test_helper:befriend(Alice, Bob),

        %% Bob sends presence
        escalus:send(Bob, escalus_stanza:presence(<<"available">>)),
        escalus:assert(is_presence, escalus:wait_for_stanza(Alice)),

        %% Bob sends presence
        escalus:send(Bob, escalus_stanza:presence(<<"unavailable">>)),
        escalus:assert(is_presence_with_type, [<<"unavailable">>],
                       escalus:wait_for_stanza(Alice))
    end).

unsubscribe(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun (Alice, Bob) ->
        test_helper:befriend(Alice, Bob),

        %% Alice sends unsubscribe
        escalus:send(Alice,
                     escalus_stanza:presence_direct(?BJID(?BOB),
                                                    <<"unsubscribe">>)),

        PushReqA2 = escalus:wait_for_stanza(Alice),
        escalus_assert:is_roster_set(PushReqA2),
        escalus:send(Alice, escalus_stanza:iq_result(PushReqA2)),

        %% Bob receives unsubscribe
        test_helper:expect_subscription_stanzas(Bob, <<"unsubscribe">>),

        %% Bob receives 'unavailable' because the subscription is no
        %% longer two-way, triggering wocky's default privacy list presence
        %% blocking
        escalus:assert(is_presence_with_type, [<<"unavailable">>],
                       escalus:wait_for_stanza(Bob))
    end).

remove_unsubscribe(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun (Alice, Bob) ->
        test_helper:befriend(Alice, Bob),

        %% remove contact
        escalus:send(Alice, escalus_stanza:roster_remove_contact(Bob)),
        IsPresUnavailable =
                fun(S) ->
                    escalus_pred:is_presence_with_type(<<"unavailable">>, S)
                end,
        escalus:assert_many([is_roster_set, is_iq_result, IsPresUnavailable],
                            escalus:wait_for_stanzas(Alice, 3)),

        %% Bob gets an unavaialble triggered by Alice changing subscription
        %% which triggers wocky's default privacy list presence blocking
        test_helper:expect_subscription_stanzas(Bob, <<"unsubscribe">>),
        escalus:assert(is_presence_with_type, [<<"unavailable">>],
                       escalus:wait_for_stanza(Bob))
    end).

subscribed_follow(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        %% Bob sends subscribed presence - this indicates that Bob
        %% has become a follower of Alice. Bob is automatically added to
        %% Alice's roster under the __new__ group.
        escalus:send(Bob, escalus_stanza:presence_direct(?BJID(?ALICE),
                                                         <<"subscribed">>)),
        Stanzas = test_helper:expect_subscription_stanzas(
                    Alice, <<"subscribed">>),
        IQ = lists:keyfind(<<"iq">>, #xmlel.name, Stanzas),
        Group = xml:get_path_s(
                  IQ, [{elem, <<"query">>},
                       {elem, <<"item">>}, {elem, <<"group">>},
                       cdata]),
        ?assertEqual(Group, <<"__new__">>)
    end).


%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------

remove_roster(Config, UserSpec) ->
    [User, _, _] = [escalus_ejabberd:unify_str_arg(Item) ||
                            Item <- escalus_users:get_usp(Config, UserSpec)],

    ok = ?wocky_roster_item:delete(User).

get_ver(Element) ->
    exml_query:path(Element, [{element, <<"query">>}, {attr, <<"ver">>}]).

has_created_at(Element) ->
    xml:get_path_s(Element, [{elem, <<"query">>},
                             {elem, <<"item">>},
                             {attr, <<"created_at">>}])
    =/= <<>>.

check_extra_fields(RosterSet) ->
    Item = exml_query:path(RosterSet, [{element, <<"query">>},
                                       {element, <<"item">>}]),
    Expected = [{<<"handle">>, ?BOB_HANDLE},
                {<<"avatar">>, ?BOB_AVATAR},
                {<<"first_name">>, ?BOB_FIRST_NAME},
                {<<"last_name">>, ?BOB_LAST_NAME}],

    ?assert(lists:all(fun(Elem) ->
                              lists:member(Elem, Item#xmlel.attrs)
                      end, Expected)),

    Roles = exml_query:paths(Item, [{element, <<"roles">>},
                                    {element, <<"role">>},
                                    cdata]),

    ?assertEqual(lists:sort(Roles), lists:sort([?ROLE1, ?ROLE2])).
