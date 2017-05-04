%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite for mod_wocky_user
-module(user_SUITE).

-compile(export_all).
-compile({parse_transform, cut}).
-compile({parse_transform, fun_chain}).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("test_helper.hrl").

-import(test_helper, [expect_iq_success/2, expect_iq_error/2]).
-import(wocky_util, [iq_id/0]).


%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, self},
     {group, other},
     {group, bulk},
%    {group, friend},
     {group, error},
     {group, set},
     {group, error_set}
    ].

groups() ->
    [
     {self, [], [all_fields,
                 some_fields,
                 garbage_get]},
     {other, [], [other_user_all_fields,
                  other_user_allowed_fields,
                  other_user_denied_field,
                  other_user_mixed_fields,
                  non_existant_user,
                  invalid_user]},
     {bulk, [], [bulk_get,
                 bulk_get_empty]},
%%     {friend, [], [friend_all_fields,
%%                   friend_allowed_fields,
%%                   friend_denied_field,
%%                   friend_mixed_fields]},
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

%% init_per_group(friend, Config) ->
%%     Users = escalus:get_users([alice, bob]),
%%     fun_chain:first(Config,
%%         escalus:create_users(Users),
%%         escalus_story:make_everyone_friends(Users)
%%     );
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
        10 = length(FieldsXML#xmlel.children)
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

other_user_all_fields(Config) ->
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
        QueryStanza = get_request(?ALICE, []),
        expect_iq_error(QueryStanza, Bob)
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

%%friend_all_fields(Config) ->
%%    escalus:story(Config, [{bob, 1}], fun(Bob) ->
%%        QueryStanza = get_request(?ALICE, []),
%%        expect_iq_error(QueryStanza, Bob)
%%    end).

%%friend_allowed_fields(Config) ->
%%    escalus:story(Config, [{bob, 1}], fun(Bob) ->
%%        QueryStanza = get_request(?ALICE,
%%                                  [<<"handle">>, <<"avatar">>,
%%                                   <<"phoneNumber">>]),
%%        expect_iq_success(QueryStanza, Bob)
%%    end).

%%friend_denied_field(Config) ->
%%    escalus:story(Config, [{bob, 1}], fun(Bob) ->
%%        QueryStanza = get_request(?ALICE, [<<"userID">>]),
%%        expect_iq_error(QueryStanza, Bob)
%%    end).

%%friend_mixed_fields(Config) ->
%%    escalus:story(Config, [{bob, 1}], fun(Bob) ->
%%        QueryStanza = get_request(?ALICE,
%%                                  [<<"uuid">>, <<"email">>, <<"userID">>]),
%%        expect_iq_error(QueryStanza, Bob)
%%    end).

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
        test_helper:subscribe_pair(Alice, Robert),
        QueryStanza = set_request(?ALICE, set_fields()),
        expect_iq_success(QueryStanza, Alice),

        #{handle := <<"Alieee">>, first_name := <<"Bob">>} =
            ?wocky_user:find(?ALICE),

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
        ?assertNot(escalus_connection:is_connected(Alice))
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

        #{id := ?BOB, first_name := nil} = ?wocky_user:find(?BOB)
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
    #xmlel{name = <<"iq">>,
           attrs = [{<<"id">>, iq_id()},
                    {<<"type">>, Type}],
           children = [#xmlel{name = Type,
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
    [jid, user, server, handle, avatar, first_name, last_name].
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
