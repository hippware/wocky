%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite for mod_wocky_user
-module(user_SUITE).

-compile(export_all).
-compile({parse_transform, cut}).
-compile({parse_transform, fun_chain}).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("test_helper.hrl").

-import(test_helper, [expect_iq_success/2, expect_iq_error/2, rsm_elem/1]).
-import(wocky_util, [iq_id/0]).


%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, self},
     {group, other},
     {group, contacts},
     {group, bulk},
     {group, error},
     {group, set},
     {group, error_set}
    ].

groups() ->
    [
     {self, [], [all_fields,
                 some_fields,
                 roles,
                 garbage_get]},
     {other, [], [other_user_all_fields,
                  other_user_allowed_fields,
                  other_user_denied_field,
                  other_user_mixed_fields,
                  non_existant_user,
                  invalid_user]},
     {contacts, [], [other_user_friends,
                     other_user_followers,
                     other_user_followees
                    ]},
     {bulk, [], [bulk_get,
                 bulk_get_empty]},
     {error, [], [missing_node,
                  malformed_user,
                  missing_user,
                  oversize_user,
                  wrong_type,
                  wrong_type2,
                  missing_var,
                  non_existant_field]},
     {set, [], [set_fields,
                set_other_user,
                garbage_set,
                delete_user]},
     {error_set, [], [set_missing_node,
                      rest_writable_field,
                      non_writable_field,
                      blank_handle,
                      set_malformed_user,
                      set_wrong_type,
                      set_missing_var,
                      set_missing_value,
                      handle_clash,
                      reserved_handle,
                      same_handle,
                      invalid_email,
                      invalid_avatar,
                      non_local_avatar,
                      non_uuid_avatar]}
    ].

suite() ->
    escalus:suite().


%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(contacts, Config) ->
    Config2 = test_helper:setup_users(Config, [alice, bob, robert]),
    ?tros_metadata:put(?AVATAR_FILE, ?ALICE, <<"all">>),
    Contacts = seed_contacts(),
    Contacts ++ Config2;
init_per_group(_GroupName, Config) ->
    Config2 = test_helper:setup_users(Config, [alice, bob, robert]),
    ?tros_metadata:put(?AVATAR_FILE, ?ALICE, <<"all">>),
    Config2.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% mod_wocky_user 'get' tests
%%--------------------------------------------------------------------

all_fields(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza = get_request(?ALICE, []),
        ResultStanza = expect_iq_success(QueryStanza, Alice),
        FieldsXML = exml_query:path(ResultStanza, [{element, <<"fields">>}]),
        NumFields = length(all_fields()),
        NumFields = length(FieldsXML#xmlel.children)
    end).

some_fields(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza = get_request(?ALICE,
                                  [<<"user">>, <<"phone_number">>,
                                   <<"external_id">>]),
        ResultStanza = expect_iq_success(QueryStanza, Alice),

        FieldsXML = exml_query:path(ResultStanza, [{element, <<"fields">>}]),
        3 = length(FieldsXML#xmlel.children),
        true = lists:any(fun(E) ->
             V = exml_query:path(E, [{element, <<"value">>}]),
             [#xmlcdata{content = ?ALICE}] =:= V#xmlel.children
           end,
           FieldsXML#xmlel.children)
    end).

roles(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        ?wocky_user:add_role(?ALICE, <<"role1">>),
        ?wocky_user:add_role(?ALICE, <<"role2">>),
        QueryStanza = get_request(?ALICE, [<<"roles">>]),
        ResultStanza = expect_iq_success(QueryStanza, Alice),

        FieldsXML = xml:get_path_s(ResultStanza, [{elem, <<"fields">>},
                                                  {elem, <<"field">>},
                                                  {elem, <<"roles">>}]),
        2 = length(FieldsXML#xmlel.children),
        RoleElems = exml_query:subelements(FieldsXML, <<"role">>),
        [<<"role1">>, <<"role2">>] = lists:sort(
                                       lists:map(
                                         xml:get_tag_cdata(_), RoleElems))
    end).

other_user_all_fields(Config) ->
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
        QueryStanza = get_request(?ALICE, []),
        ResultStanza = expect_iq_success(QueryStanza, Bob),
        FieldsXML = exml_query:path(ResultStanza, [{element, <<"fields">>}]),
        NumFields = length(public_fields()),
        NumFields = length(FieldsXML#xmlel.children),
        expect_results(public_fields(), FieldsXML#xmlel.children)
    end).

other_user_allowed_fields(Config) ->
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
        QueryStanza = get_request(?ALICE,
                                  [<<"handle">>, <<"avatar">>]),
        expect_iq_success(QueryStanza, Bob)
    end).

other_user_denied_field(Config) ->
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
        QueryStanza = get_request(?ALICE, [<<"phone_number">>]),
        expect_iq_error(QueryStanza, Bob)
    end).

other_user_mixed_fields(Config) ->
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
        QueryStanza = get_request(?ALICE,
                                  [<<"user">>, <<"email">>,
                                   <<"phone_number">>]),
        expect_iq_error(QueryStanza, Bob)
    end).

non_existant_user(Config) ->
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
        QueryStanza = get_request(?wocky_id:new(),
                                  [<<"handle">>]),
        expect_iq_error(QueryStanza, Bob)
    end).

invalid_user(Config) ->
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
        QueryStanza = get_request(<<"non-uuid-user">>,
                                  [<<"handle">>]),
        expect_iq_error(QueryStanza, Bob)
    end).

other_user_friends(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        fun_chain:first(
          ?BOB,
          contact_request(<<"friend">>, #rsm_in{}),
          expect_iq_success(Alice),
          check_returned_contacts(Config, [friend])
         )
    end).

other_user_followers(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        fun_chain:first(
          ?BOB,
          contact_request(<<"follower">>, #rsm_in{}),
          expect_iq_success(Alice),
          check_returned_contacts(Config, [friend, follower])
         )
    end).

other_user_followees(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        fun_chain:first(
          ?BOB,
          contact_request(<<"following">>, #rsm_in{}),
          expect_iq_success(Alice),
          check_returned_contacts(Config, [friend, following])
         )
    end).

missing_node(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza = get_request(?ALICE,
                                  [<<"uuid">>, <<"email">>, <<"external_id">>]),
        Attrs = (hd(QueryStanza#xmlel.children))#xmlel.attrs,
        BrokenAttrs = proplists:delete(<<"node">>, Attrs),
        BrokenStanza =
        QueryStanza#xmlel{children =
                          [(hd(QueryStanza#xmlel.children))#xmlel{attrs =
                                                             BrokenAttrs}]},
        expect_iq_error(BrokenStanza, Alice)
    end).

malformed_user(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza = get_request(?ALICE,
                                  [<<"user">>, <<"email">>, <<"external_id">>]),
        Attrs = (hd(QueryStanza#xmlel.children))#xmlel.attrs,
        BrokenAttrs = [{<<"node">>, <<"baduserbad">>} |
                       proplists:delete(<<"node">>, Attrs)],
        BrokenStanza =
        QueryStanza#xmlel{children =
                          [(hd(QueryStanza#xmlel.children))#xmlel{attrs =
                                                             BrokenAttrs}]},
        expect_iq_error(BrokenStanza, Alice)
    end).

missing_user(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza = get_request(?ALICE,
                                  [<<"user">>, <<"email">>, <<"external_id">>]),
        Attrs = (hd(QueryStanza#xmlel.children))#xmlel.attrs,
        BrokenAttrs = [{<<"node">>, <<"user/">>} |
                       proplists:delete(<<"node">>, Attrs)],
        BrokenStanza =
        QueryStanza#xmlel{children =
                          [(hd(QueryStanza#xmlel.children))#xmlel{attrs =
                                                             BrokenAttrs}]},
        expect_iq_error(BrokenStanza, Alice)
    end).

oversize_user(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza = get_request(?ALICE,
                                  [<<"user">>, <<"email">>, <<"external_id">>]),
        Attrs = (hd(QueryStanza#xmlel.children))#xmlel.attrs,
        BigUser = binary:copy(<<"a">>, 2048),
        BrokenAttrs = [{<<"node">>, <<"user/", BigUser/binary>>} |
                       proplists:delete(<<"node">>, Attrs)],
        BrokenStanza =
        QueryStanza#xmlel{children =
                          [(hd(QueryStanza#xmlel.children))#xmlel{attrs =
                                                             BrokenAttrs}]},
        expect_iq_error(BrokenStanza, Alice)
    end).

wrong_type(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza = get_request(?ALICE, []),
        BrokenStanza =
        QueryStanza#xmlel{attrs = lists:keyreplace(<<"type">>, 1,
                                                   QueryStanza#xmlel.attrs,
                                                   {<<"type">>, <<"fnord">>})},
        expect_iq_error(BrokenStanza, Alice)
    end).

missing_var(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        BrokenStanza =
        request_wrapper(<<"get">>, ?ALICE,
                        [#xmlel{name = <<"field">>}]),
        expect_iq_error(BrokenStanza, Alice)
    end).


non_existant_field(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza = get_request(?ALICE,
                                  [<<"user">>, <<"doesntexist">>]),
        expect_iq_error(QueryStanza, Alice)
    end).

wrong_type2(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza = invalid_request(?ALICE,
                                      [<<"user">>, <<"doesntexist">>]),
        expect_iq_error(QueryStanza, Alice)
    end).

garbage_get(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza = garbage_request(?ALICE, <<"get">>),
        % No get fields = get all, so this one still works:
        expect_iq_success(QueryStanza, Alice)
    end).

%%--------------------------------------------------------------------
%% mod_wocky_user 'users' (bulk) tests
%%--------------------------------------------------------------------

bulk_get(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
      #xmlel{name = <<"iq">>,
             children = [Result]}
      = expect_iq_success(
                 users_request(
                   [?BJID(?ALICE), ?BJID(?BOB), ?BJID(?TIM),
                    <<"xxx">>, <<"xxx@123">>, <<>>]),
                 Alice),
      expect_bulk_results(Result,
                          [{?BJID(?ALICE), all_fields()},
                           {?BJID(?BOB), public_fields()},
                           {?BJID(?TIM), error},
                           {<<"xxx@123">>, error},
                           {<<>>, error},
                           {<<"xxx">>, error}
                          ])
    end).

bulk_get_empty(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
      #xmlel{name = <<"iq">>, children = [Result]}
      = expect_iq_success(users_request([]), Alice),
      expect_bulk_results(Result, [])
    end).

%%--------------------------------------------------------------------
%% mod_wocky_user 'set' tests
%%--------------------------------------------------------------------

set_fields(Config) ->
    escalus:story(Config, [{alice, 1}, {robert, 1}], fun(Alice, Robert) ->
        test_helper:befriend(Alice, Robert),
        QueryStanza = set_request(?ALICE, set_fields()),
        expect_iq_success(QueryStanza, Alice),

        #{handle := <<"Alieee">>, first_name := <<"Bob">>} =
            ?wocky_repo:get(?wocky_user, ?ALICE),

        % Robert should get an update for his roster record of Alice
        Received = escalus:wait_for_stanza(Robert),
        escalus:assert(is_roster_set, Received),
        ?assertEqual(<<"Alieee">>,
                     xml:get_path_s(Received, [{elem, <<"query">>},
                                               {elem, <<"item">>},
                                               {attr, <<"handle">>}]))
    end).

set_other_user(Config) ->
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
        QueryStanza = set_request(?ALICE, set_fields()),
        expect_iq_error(QueryStanza, Bob)
    end).

set_missing_node(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza = set_request(?ALICE, set_fields()),
        Attrs = (hd(QueryStanza#xmlel.children))#xmlel.attrs,
        BrokenAttrs = proplists:delete(<<"node">>, Attrs),
        BrokenStanza =
            QueryStanza#xmlel{children =
                              [(hd(QueryStanza#xmlel.children))#xmlel{attrs =
                                                                 BrokenAttrs}]},
        expect_iq_error(BrokenStanza, Alice)
    end).

delete_user(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza = delete_request(),
        expect_iq_success(QueryStanza, Alice),
        R = escalus:wait_for_stanza(Alice, 3000),
        escalus:assert(is_stream_error,
                       [<<"conflict">>, <<"User removed">>], R),
        timer:sleep(500),
        ?assertNot(escalus_connection:is_connected(Alice)),
        ?assertEqual(?wocky_repo:get(?wocky_user, ?ALICE), nil)
    end).

%%--------------------------------------------------------------------
%% mod_wocky_user 'set' tests with errors
%%--------------------------------------------------------------------

rest_writable_field(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza =
            set_request(?ALICE,
                        [{<<"phone_number">>, <<"string">>, <<"+1444">>}]),
        expect_iq_error(QueryStanza, Alice)
    end).

non_writable_field(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza =
            set_request(?ALICE,
                        [{<<"user">>, <<"uuid">>, ?wocky_id:new()}]),
        expect_iq_error(QueryStanza, Alice)
    end).

blank_handle(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza =
            set_request(?ALICE,
                        [{<<"handle">>, <<"string">>, <<"">>}]),
        expect_iq_error(QueryStanza, Alice)
    end).


set_malformed_user(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza = set_request(?ALICE, set_fields()),
        Attrs = (hd(QueryStanza#xmlel.children))#xmlel.attrs,
        BrokenAttrs = [{<<"node">>, <<"ack-fnord">>} |
                       proplists:delete(<<"node">>, Attrs)],
        BrokenStanza =
            QueryStanza#xmlel{children =
                              [(hd(QueryStanza#xmlel.children))#xmlel{attrs =
                                                                 BrokenAttrs}]},
        expect_iq_error(BrokenStanza, Alice)
    end).

set_wrong_type(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza =
            request_wrapper(<<"set">>, ?ALICE,
                            [#xmlel{name = <<"field">>,
                                    attrs = [{<<"var">>, <<"first_name">>},
                                             {<<"type">>, <<"strOng">>}]
                                   }]),
        expect_iq_error(QueryStanza, Alice)
    end).

set_missing_var(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        BrokenStanza =
            request_wrapper(<<"set">>, ?ALICE,
                            [#xmlel{name = <<"field">>}]),
        expect_iq_error(BrokenStanza, Alice)
    end).

set_missing_value(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        BrokenStanza =
            request_wrapper(<<"set">>, ?ALICE,
                            [#xmlel{name = <<"field">>,
                                    attrs = [{<<"var">>, <<"first_name">>},
                                             {<<"type">>, <<"string">>}],
                                    children = [#xmlel{name = <<"other">>}]
                                   }]),
        expect_iq_error(BrokenStanza, Alice)
    end).

handle_clash(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        QueryStanza = set_request(?ALICE, set_fields()),
        expect_iq_success(QueryStanza, Alice),
        BobQueryStanza = set_request(?BOB, set_fields()),
        expect_iq_error(BobQueryStanza, Bob),

        #{id := ?BOB, first_name := nil} = ?wocky_repo:get(?wocky_user, ?BOB)
    end).

reserved_handle(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        %% Reserved names should be rejected
        QueryStanza =
            set_request(?ALICE,
                        [{<<"handle">>, <<"string">>, <<"root">>}]),
        expect_iq_error(QueryStanza, Alice),

        %% Check with alternative capitalisation
        QueryStanza2 =
            set_request(?ALICE,
                        [{<<"handle">>, <<"string">>, <<"rOOt">>}]),
        expect_iq_error(QueryStanza2, Alice)
    end).

same_handle(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza = set_request(?ALICE, set_fields()),
        expect_iq_success(QueryStanza, Alice)
    end).

garbage_set(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza = garbage_request(?ALICE, <<"set">>),
        % Successfully set nothing:
        expect_iq_success(QueryStanza, Alice)
    end).

invalid_email(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza =
            set_request(?ALICE,
                        [{<<"email">>, <<"string">>, <<"notanemail">>}]),
        expect_iq_error(QueryStanza, Alice)
    end).

invalid_avatar(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza =
            set_request(?ALICE,
                        [{<<"avatar">>, <<"file">>, <<"notaURL">>}]),
        expect_iq_error(QueryStanza, Alice)
    end).

non_local_avatar(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza =
            set_request(?ALICE,
                        [{<<"avatar">>, <<"file">>,
                          <<"tros:user@otherserver.com/file/",
                            ?AVATAR_FILE/binary>>}]),
        expect_iq_error(QueryStanza, Alice)
    end).

non_uuid_avatar(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza =
            set_request(?ALICE,
                        [{<<"avatar">>, <<"file">>,
                          <<"tros:", (?ALICE)/binary, "@",
                            ?SERVER/binary, "/file/blahblah">>}]),
        expect_iq_error(QueryStanza, Alice)
    end).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

request_wrapper(Type, User, DataFields) ->
    request_wrapper(Type, Type, User, DataFields).

request_wrapper(IQType, Request, User, DataFields) ->
    #xmlel{name = <<"iq">>,
           attrs = [{<<"id">>, iq_id()},
                    {<<"type">>, IQType}],
           children = [#xmlel{name = Request,
                              attrs = [{<<"xmlns">>,
                                        ?NS_USER},
                                       {<<"node">>,
                                        <<"user/", User/binary>>}],
                              children = DataFields
                             }]}.

get_request(User, Fields) ->
    ReqFields = [#xmlel{name = <<"field">>, attrs = [{<<"var">>, F}]}
                 || F <- Fields],
    request_wrapper(<<"get">>, User, ReqFields).

set_request(User, Fields) ->
    ReqFields = [#xmlel{name = <<"field">>,
                        attrs = [{<<"var">>, Var},
                                 {<<"type">>, Type}],
                        children = [#xmlel{name = <<"value">>,
                                           children =
                                           [#xmlcdata{content = Value}]}]}
                 || {Var, Type, Value} <- Fields],
    request_wrapper(<<"set">>, User, ReqFields).

contact_request(UserID, Association, RSMIn) ->
    ReqFields = [wocky_xml:cdata_el(<<"association">>, Association),
                 rsm_elem(RSMIn)],
    request_wrapper(<<"get">>, <<"contacts">>, UserID, ReqFields).

invalid_request(User, Fields) ->
    ReqFields = [#xmlel{name = <<"field">>, attrs = [{<<"var">>, F}]}
                 || F <- Fields],
    request_wrapper(<<"Bad">>, User, ReqFields).

garbage_request(User, Type) ->
    ReqFields = [#xmlel{name = <<"sdkj">>, attrs = [{<<"arr">>, <<"d">>}]}],
    request_wrapper(Type, User, ReqFields).

set_fields() ->
    [{<<"handle">>, <<"string">>, <<"Alieee">>},
     {<<"first_name">>, <<"string">>, <<"Bob">>},
     {<<"email">>, <<"string">>, <<"bob@alice.com">>},
     {<<"tagline">>, <<"string">>, <<"Tag Line">>},
     {<<"avatar">>, <<"file">>,
      <<"tros:", ?ALICE/binary, "@",
        ?SERVER/binary, "/file/", ?AVATAR_FILE/binary>>}].

delete_request() ->
    #xmlel{name = <<"iq">>,
           attrs = [{<<"id">>, iq_id()},
                    {<<"type">>, <<"set">>}],
           children = [#xmlel{name = <<"delete">>,
                              attrs = [{<<"xmlns">>, ?NS_USER}]
                             }]}.

users_request(BJIDs) ->
    Users = [#xmlel{name = <<"user">>, attrs = [{<<"jid">>, B}]} || B <- BJIDs],
    test_helper:iq_get(?NS_USER, #xmlel{name = <<"users">>, children = Users}).

public_fields() ->
    [jid, user, server, handle, avatar, first_name, last_name, tagline,
     'bots+size', 'followers+size', 'followed+size', roles].
private_fields() -> [phone_number, email, external_id].
all_fields() -> public_fields() ++ private_fields().

expect_bulk_results(#xmlel{name = <<"users">>, children = Children}, Results) ->
    ?assertEqual([], lists:foldl(expect_bulk_result(_, _), Results, Children)).

expect_bulk_result(
  #xmlel{name = <<"user">>, attrs = [{<<"jid">>, JID}], children = Children},
  UnmatchedResults) ->
    {value, {JID, ExpectedResult}, UnmatchedResults2} =
    lists:keytake(JID, 1, UnmatchedResults),
    expect_results(ExpectedResult, Children),
    UnmatchedResults2.

expect_results(error, [#xmlel{name = <<"error">>}]) -> ok;
expect_results(ExpectedFields, Fields)
  when length(ExpectedFields) =:= length(Fields) ->
    lists:foreach(fun(F) ->
                          ?assert(has_field(Fields, F))
                  end, ExpectedFields).

has_field(Fields, Field) ->
    FieldBin = atom_to_binary(Field, utf8),
    lists:any(fun(El = #xmlel{name = <<"field">>}) ->
                      exml_query:path(El, [{attr, <<"var">>}]) =:= FieldBin
              end, Fields).

seed_contacts() ->
    NilHandleUser = ?wocky_factory:insert(user,
                                          #{server => ?SERVER, handle => nil}),
    Friends = ?wocky_factory:insert_list(4, user, #{server => ?SERVER}),
    lists:foreach(insert_friend_pair(?BOB, _), ids([NilHandleUser | Friends])),

    Followers = ?wocky_factory:insert_list(5, user, #{server => ?SERVER}),
    lists:foreach(insert_follower_pair(_, ?BOB), ids(Followers)),

    Followees = ?wocky_factory:insert_list(5, user, #{server => ?SERVER}),
    lists:foreach(insert_follower_pair(?BOB, _), ids(Followees)),

    SystemUser = test_helper:insert_system_users(),
    insert_friend_pair(?BOB, maps:get(id, SystemUser)),

    [{friend, Friends}, {follower, Followers},
     {following, Followees}, {system, SystemUser}].

ids(UserList) ->
    [maps:get(id, U) || U <- UserList].

insert_follower_pair(Follower, Followee) ->
    insert_roster_pair(Follower, Followee, from, to).

insert_friend_pair(User1, User2) ->
    insert_roster_pair(User1, User2, both, both).

insert_roster_pair(User1, User2, Dir1, Dir2) ->
    ?wocky_factory:insert(roster_item,
                          #{subscription => Dir1,
                            contact_id => User1,
                            user_id => User2}),
    ?wocky_factory:insert(roster_item,
                          #{subscription => Dir2,
                            contact_id => User2,
                            user_id => User1}).

check_returned_contacts(Stanza, Config, Sets) ->
    Users = lists:flatten([add_associations(proplists:get_value(S, Config), S)
                           || S <- Sets]),

    Contacts = extract_contacts(Stanza),
    match_contacts(Contacts, Users).

add_associations(Users, Association) ->
    lists:map(fun(U) -> U#{association => Association} end, Users).

extract_contacts(#xmlel{name = <<"iq">>,
                        children = [#xmlel{name = <<"contacts">>,
                                           children = Children}]}) ->
    lists:foldl(extract_contact(_, _), [], Children).

extract_contact(#xmlel{name = <<"contact">>, attrs = Attrs}, Acc) ->
    [#{jid => proplists:get_value(<<"jid">>, Attrs),
       handle => proplists:get_value(<<"handle">>, Attrs),
       association => proplists:get_value(<<"association">>, Attrs)}
     | Acc];
extract_contact(_, Acc) -> lists:reverse(Acc).

match_contacts([], []) -> ok;
match_contacts(Contacts, [#{handle := UHandle, id := ID, server := Server,
                            association := UAssociation} | Users]) ->
    CJID = jid:to_binary(jid:make(ID, Server, <<>>)),
    CAssociation = atom_to_binary(UAssociation, utf8),
    Remaining = lists:filter(
                  fun(#{handle := H, jid := J, association := A}) ->
                          not (
                            (H =:= UHandle orelse
                             (H =:= <<>> andalso UHandle =:= nil)) andalso
                            J =:= CJID andalso
                            A =:= CAssociation
                           )
                  end, Contacts),
    ?assert(length(Remaining) =:= length(Contacts) - 1),
    match_contacts(Remaining, Users);
match_contacts(Contacts, Users) ->
    ct:fail("Failed to match ~p to ~p", [Contacts, Users]).
