%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite for mod_wocky_user
-module(user_SUITE).
-compile(export_all).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("common_test/include/ct.hrl").

-include("wocky_db_seed.hrl").

-define(ALICE_UUID, escalus_users:get_username(Config, alice)).
-define(BOB_UUID, escalus_users:get_username(Config, bob)).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, self},
     {group, other},
%    {group, friend},
     {group, error},
     {group, set},
     {group, error_set}
    ].

groups() ->
    [
     {self, [sequence], [all_fields,
                         some_fields,
                         garbage_get
                        ]},
     {other, [sequence], [other_user_all_fields,
                          other_user_allowed_fields,
                          other_user_denied_field,
                          other_user_mixed_fields,
                          non_existant_user,
                          invalid_user]},
%     {friend, [sequence], [friend_all_fields,
%                          friend_allowed_fields,
%                          friend_denied_field,
%                          friend_mixed_fields]},
     {error, [sequence], [missing_node,
                          malformed_user,
                          wrong_type,
                          wrong_type2,
                          missing_var,
                          non_existant_field
                         ]},
     {set, [sequence], [set_fields,
                        set_other_user,
                        garbage_set
                       ]},
     {error_set, [sequence], [set_missing_node,
                              rest_writable_field,
                              non_writable_field,
                              blank_handle,
                              set_malformed_user,
                              set_wrong_type,
                              set_missing_var,
                              set_missing_value,
                              handle_clash
                             ]}
    ].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    test_helper:start_ejabberd(),
    wocky_db_seed:clear_user_tables(?LOCAL_CONTEXT),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config),
    test_helper:stop_ejabberd(),
    ok.

init_per_group(friend, Config) ->
    Config2 = escalus:create_users(Config),
    escalus:make_everyone_friends(Config2),
    escalus_ejabberd:wait_for_session_count(Config2, 0),
    Config2;
init_per_group(_GroupName, Config) ->
    escalus:create_users(Config),
    escalus_ejabberd:wait_for_session_count(Config, 0),
    Config.

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% mod_wocky_user 'get' tests
%%--------------------------------------------------------------------

all_fields(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza = get_request(<<"456">>, ?ALICE_UUID,
                                  []),
        ResultStanza = expect_success(Config, QueryStanza, Alice, alice),
        FieldsXML = exml_query:path(ResultStanza, [{element, <<"fields">>}]),
        11 = length(FieldsXML#xmlel.children)
    end).

some_fields(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza = get_request(<<"457">>, ?ALICE_UUID,
                                  [<<"uuid">>, <<"phoneNumber">>,
                                   <<"userID">>]),
        ResultStanza = expect_success(Config, QueryStanza, Alice, alice),

        FieldsXML = exml_query:path(ResultStanza, [{element, <<"fields">>}]),
        3 = length(FieldsXML#xmlel.children),
        true = lists:any(fun(E) ->
             V = exml_query:path(E, [{element, <<"value">>}]),
             [#xmlcdata{content = ?ALICE_UUID}] =:= V#xmlel.children
           end,
           FieldsXML#xmlel.children)
    end).

other_user_all_fields(Config) ->
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
        QueryStanza = get_request(<<"458">>, ?ALICE_UUID, []),
        expect_error(Config, QueryStanza, Bob, bob)
    end).

other_user_allowed_fields(Config) ->
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
        QueryStanza = get_request(<<"459">>, ?ALICE_UUID,
                                  [<<"handle">>, <<"avatar">>]),
        expect_success(Config, QueryStanza, Bob, bob)
    end).

other_user_denied_field(Config) ->
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
        QueryStanza = get_request(<<"460">>, ?ALICE_UUID, [<<"phoneNumber">>]),
        expect_error(Config, QueryStanza, Bob, bob)
    end).

other_user_mixed_fields(Config) ->
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
        QueryStanza = get_request(<<"461">>, ?ALICE_UUID,
                                  [<<"uuid">>, <<"email">>, <<"phoneNumber">>]),
        expect_error(Config, QueryStanza, Bob, bob)
    end).

non_existant_user(Config) ->
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
        QueryStanza = get_request(<<"462">>, wocky_db:create_id(),
                                  [<<"handle">>]),
        expect_error(Config, QueryStanza, Bob, bob)
    end).

invalid_user(Config) ->
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
        QueryStanza = get_request(<<"462">>,
                                  <<"non-uuid-user">>,
                                  [<<"handle">>]),
        expect_error(Config, QueryStanza, Bob, bob)
    end).

%friend_all_fields(Config) ->
%    escalus:story(Config, [{bob, 1}], fun(Bob) ->
%        QueryStanza = get_request(<<"463">>, ?ALICE_UUID, []),
%        expect_error(Config, QueryStanza, Bob, bob)
%    end).

%friend_allowed_fields(Config) ->
%    escalus:story(Config, [{bob, 1}], fun(Bob) ->
%        QueryStanza = get_request(<<"464">>, ?ALICE_UUID,
%                                  [<<"handle">>, <<"avatar">>,
%                                   <<"phoneNumber">>]),
%        expect_success(Config, QueryStanza, Bob, bob)
%    end).

%friend_denied_field(Config) ->
%    escalus:story(Config, [{bob, 1}], fun(Bob) ->
%        QueryStanza = get_request(<<"465">>, ?ALICE_UUID, [<<"userID">>]),
%        expect_error(Config, QueryStanza, Bob, bob)
%    end).

%friend_mixed_fields(Config) ->
%    escalus:story(Config, [{bob, 1}], fun(Bob) ->
%        QueryStanza = get_request(<<"466">>, ?ALICE_UUID,
%                                  [<<"uuid">>, <<"email">>, <<"userID">>]),
%        expect_error(Config, QueryStanza, Bob, bob)
%    end).

missing_node(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza = get_request(<<"467">>, ?ALICE_UUID,
                                  [<<"uuid">>, <<"email">>, <<"userID">>]),
        Attrs = (hd(QueryStanza#xmlel.children))#xmlel.attrs,
        BrokenAttrs = proplists:delete(<<"node">>, Attrs),
        BrokenStanza =
        QueryStanza#xmlel{children =
                          (hd(QueryStanza#xmlel.children))#xmlel{attrs =
                                                             BrokenAttrs}},
        expect_error(Config, BrokenStanza, Alice, alice)
    end).

malformed_user(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza = get_request(<<"468">>, ?ALICE_UUID,
                                  [<<"uuid">>, <<"email">>, <<"userID">>]),
        Attrs = (hd(QueryStanza#xmlel.children))#xmlel.attrs,
        BrokenAttrs = [{<<"node">>, <<"baduserbad">>} |
                       proplists:delete(<<"node">>, Attrs)],
        BrokenStanza =
        QueryStanza#xmlel{children =
                          (hd(QueryStanza#xmlel.children))#xmlel{attrs =
                                                             BrokenAttrs}},
        expect_error(Config, BrokenStanza, Alice, alice)
    end).

wrong_type(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza = get_request(<<"469">>, ?ALICE_UUID, []),
        BrokenStanza =
        QueryStanza#xmlel{attrs = lists:keyreplace(<<"type">>, 1,
                                                   QueryStanza#xmlel.attrs,
                                                   {<<"type">>, <<"fnord">>})},
        expect_error(Config, BrokenStanza, Alice, alice)
    end).

missing_var(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        BrokenStanza =
        request_wrapper(<<"470">>,
                        <<"get">>, ?ALICE_UUID,
                        [#xmlel{name = <<"field">>}]),
        expect_error(Config, BrokenStanza, Alice, alice)
    end).


non_existant_field(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza = get_request(<<"471">>, ?ALICE_UUID,
                                  [<<"uuid">>, <<"doesntexist">>]),
        expect_error(Config, QueryStanza, Alice, alice)
    end).

wrong_type2(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza = invalid_request(<<"472">>, ?ALICE_UUID,
                                      [<<"uuid">>, <<"doesntexist">>]),
        expect_error(Config, QueryStanza, Alice, alice)
    end).

garbage_get(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza = garbage_request(<<"473">>, ?ALICE_UUID, <<"get">>),
        % No get fields = get all, so this one still works:
        expect_success(Config, QueryStanza, Alice, alice)
    end).

%%--------------------------------------------------------------------
%% mod_wocky_user 'set' tests
%%--------------------------------------------------------------------

set_fields(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza =
        set_request(<<"571">>, ?ALICE_UUID, set_fields()),
        expect_success(Config, QueryStanza, Alice, alice),

        #{handle := <<"Alieee">>, first_name := <<"Bob">>} =
        wocky_db_user:get_user_data(?ALICE_UUID, ?LOCAL_CONTEXT)
    end).

set_other_user(Config) ->
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
        QueryStanza =
        set_request(<<"572">>, ?ALICE_UUID, set_fields()),
        expect_error(Config, QueryStanza, Bob, bob)
    end).

set_missing_node(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza =
        set_request(<<"573">>, ?ALICE_UUID, set_fields()),
        Attrs = (hd(QueryStanza#xmlel.children))#xmlel.attrs,
        BrokenAttrs = proplists:delete(<<"node">>, Attrs),
        BrokenStanza =
        QueryStanza#xmlel{children =
                          (hd(QueryStanza#xmlel.children))#xmlel{attrs =
                                                             BrokenAttrs}},
        expect_error(Config, BrokenStanza, Alice, alice)
    end).

rest_writable_field(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza =
        set_request(<<"574">>, ?ALICE_UUID,
                    [{<<"phoneNumber">>, <<"string">>, <<"+1444">>}]),
        expect_error(Config, QueryStanza, Alice, alice)
    end).

non_writable_field(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza =
        set_request(<<"575">>, ?ALICE_UUID,
                    [{<<"uuid">>, <<"uuid">>, wocky_db:create_id()}]),
        expect_error(Config, QueryStanza, Alice, alice)
    end).

blank_handle(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza =
        set_request(<<"576">>, ?ALICE_UUID,
                    [{<<"handle">>, <<"string">>, <<"">>}]),
        expect_error(Config, QueryStanza, Alice, alice)
    end).


set_malformed_user(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza =
        set_request(<<"577">>, ?ALICE_UUID, set_fields()),
        Attrs = (hd(QueryStanza#xmlel.children))#xmlel.attrs,
        BrokenAttrs = proplists:delete(<<"node">>, Attrs),
        BrokenStanza =
        QueryStanza#xmlel{children =
                          (hd(QueryStanza#xmlel.children))#xmlel{attrs =
                                                             BrokenAttrs}},
        expect_error(Config, BrokenStanza, Alice, alice)
    end).

set_wrong_type(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza =
        request_wrapper(<<"578">>, <<"set">>, ?ALICE_UUID,
                        [#xmlel{name = <<"field">>,
                                attrs = [{<<"var">>, <<"firstName">>},
                                         {<<"type">>, <<"strOng">>}]
                               }]),
        expect_error(Config, QueryStanza, Alice, alice)
    end).

set_missing_var(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        BrokenStanza =
        request_wrapper(<<"579">>, <<"set">>, ?ALICE_UUID,
                        [#xmlel{name = <<"field">>}]),
        expect_error(Config, BrokenStanza, Alice, alice)
    end).

set_missing_value(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        BrokenStanza =
        request_wrapper(<<"580">>, <<"set">>, ?ALICE_UUID,
                        [#xmlel{name = <<"field">>,
                                attrs = [{<<"var">>, <<"firstName">>},
                                         {<<"type">>, <<"string">>}],
                                children = [#xmlel{name = <<"other">>}]
                               }]),
        expect_error(Config, BrokenStanza, Alice, alice)
    end).

handle_clash(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        QueryStanza =
        set_request(<<"581">>, ?ALICE_UUID, set_fields()),
        expect_success(Config, QueryStanza, Alice, alice),
        BobQueryStanza =
        set_request(<<"582">>, ?BOB_UUID, set_fields()),
        expect_error(Config, BobQueryStanza, Bob, bob),

        BobUUID = ?BOB_UUID,
        #{handle := BobUUID, first_name := null} =
        wocky_db_user:get_user_data(?BOB_UUID, ?LOCAL_CONTEXT)
    end).

garbage_set(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza =
        garbage_request(<<"583">>, ?ALICE_UUID, <<"set">>),
        % Successfully set nothing:
        expect_success(Config, QueryStanza, Alice, alice)
    end).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------


request_wrapper(ID, Type, User, DataFields) ->
    #xmlel{name = <<"iq">>,
           attrs = [{<<"id">>, ID},
                    {<<"type">>, Type}],
           children = [#xmlel{name = Type,
                              attrs = [{<<"xmlns">>,
                                        <<"hippware.com/hxep/user">>},
                                       {<<"node">>,
                                        <<"user/", User/binary>>}],
                              children = DataFields
                             }]}.

get_request(ID, User, Fields) ->
    ReqFields = [#xmlel{name = <<"field">>, attrs = [{<<"var">>, F}]}
                 || F <- Fields],
    request_wrapper(ID, <<"get">>, User, ReqFields).

set_request(ID, User, Fields) ->
    ReqFields = [#xmlel{name = <<"field">>,
                        attrs = [{<<"var">>, Var},
                                 {<<"type">>, Type}],
                        children = [#xmlel{name = <<"value">>,
                                           children =
                                           [#xmlcdata{content = Value}]}]}
                 || {Var, Type, Value} <- Fields],
    request_wrapper(ID, <<"set">>, User, ReqFields).

invalid_request(ID, User, Fields) ->
    ReqFields = [#xmlel{name = <<"field">>, attrs = [{<<"var">>, F}]}
                 || F <- Fields],
    request_wrapper(ID, <<"Bad">>, User, ReqFields).

garbage_request(ID, User, Type) ->
    ReqFields = [#xmlel{name = <<"sdkj">>, attrs = [{<<"arr">>, <<"d">>}]}],
    request_wrapper(ID, Type, User, ReqFields).

set_fields() ->
    [{<<"handle">>, <<"string">>, <<"Alieee">>},
     {<<"firstName">>, <<"string">>, <<"Bob">>}].

add_to_from(Config, Stanza, User) ->
    escalus_stanza:to(
      escalus_stanza:from(Stanza, User),
      escalus_users:get_server(Config, User)).

expect_error(Config, Stanza, User, UserID) ->
    FinalStanza = add_to_from(Config, Stanza, UserID),
    ResultStanza = escalus:send_and_wait(User, FinalStanza),
    escalus:assert(is_iq_error, ResultStanza),
    ResultStanza.

expect_success(Config, Stanza, User, UserID) ->
    FinalStanza = add_to_from(Config, Stanza, UserID),
    ResultStanza = escalus:send_and_wait(User, FinalStanza),
    escalus:assert(is_iq_result, [FinalStanza], ResultStanza),
    ResultStanza.
