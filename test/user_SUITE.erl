%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite for mod_wocky_user
-module(user_SUITE).
-compile(export_all).
-compile({parse_transform, fun_chain}).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("wocky_db_seed.hrl").

-define(ALICE_UUID, escalus_users:get_username(Config, alice)).
-define(BOB_UUID, escalus_users:get_username(Config, bob)).

-import(test_helper, [expect_iq_success/2, expect_iq_error/2]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, self},
     {group, other},
%%    {group, friend},
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
%%     {friend, [], [friend_all_fields,
%%                   friend_allowed_fields,
%%                   friend_denied_field,
%%                   friend_mixed_fields]},
     {error, [], [missing_node,
                  malformed_user,
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
    wocky_db_seed:clear_user_tables(?LOCAL_CONTEXT),
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
    escalus:create_users(Config, escalus:get_users([alice, bob])).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob])).

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
        ResultStanza = expect_iq_success(QueryStanza, Alice),
        FieldsXML = exml_query:path(ResultStanza, [{element, <<"fields">>}]),
        10 = length(FieldsXML#xmlel.children)
    end).

some_fields(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza = get_request(<<"457">>, ?ALICE_UUID,
                                  [<<"user">>, <<"phone_number">>,
                                   <<"external_id">>]),
        ResultStanza = expect_iq_success(QueryStanza, Alice),

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
        expect_iq_error(QueryStanza, Bob)
    end).

other_user_allowed_fields(Config) ->
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
        QueryStanza = get_request(<<"459">>, ?ALICE_UUID,
                                  [<<"handle">>, <<"avatar">>]),
        expect_iq_success(QueryStanza, Bob)
    end).

other_user_denied_field(Config) ->
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
        QueryStanza = get_request(<<"460">>, ?ALICE_UUID, [<<"phone_number">>]),
        expect_iq_error(QueryStanza, Bob)
    end).

other_user_mixed_fields(Config) ->
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
        QueryStanza = get_request(<<"461">>, ?ALICE_UUID,
                                  [<<"user">>, <<"email">>,
                                   <<"phone_number">>]),
        expect_iq_error(QueryStanza, Bob)
    end).

non_existant_user(Config) ->
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
        QueryStanza = get_request(<<"462">>, wocky_db:create_id(),
                                  [<<"handle">>]),
        expect_iq_error(QueryStanza, Bob)
    end).

invalid_user(Config) ->
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
        QueryStanza = get_request(<<"462">>,
                                  <<"non-uuid-user">>,
                                  [<<"handle">>]),
        expect_iq_error(QueryStanza, Bob)
    end).

%%friend_all_fields(Config) ->
%%    escalus:story(Config, [{bob, 1}], fun(Bob) ->
%%        QueryStanza = get_request(<<"463">>, ?ALICE_UUID, []),
%%        expect_iq_error(QueryStanza, Bob)
%%    end).

%%friend_allowed_fields(Config) ->
%%    escalus:story(Config, [{bob, 1}], fun(Bob) ->
%%        QueryStanza = get_request(<<"464">>, ?ALICE_UUID,
%%                                  [<<"handle">>, <<"avatar">>,
%%                                   <<"phoneNumber">>]),
%%        expect_iq_success(QueryStanza, Bob)
%%    end).

%%friend_denied_field(Config) ->
%%    escalus:story(Config, [{bob, 1}], fun(Bob) ->
%%        QueryStanza = get_request(<<"465">>, ?ALICE_UUID, [<<"userID">>]),
%%        expect_iq_error(QueryStanza, Bob)
%%    end).

%%friend_mixed_fields(Config) ->
%%    escalus:story(Config, [{bob, 1}], fun(Bob) ->
%%        QueryStanza = get_request(<<"466">>, ?ALICE_UUID,
%%                                  [<<"uuid">>, <<"email">>, <<"userID">>]),
%%        expect_iq_error(QueryStanza, Bob)
%%    end).

missing_node(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza = get_request(<<"467">>, ?ALICE_UUID,
                                  [<<"uuid">>, <<"email">>, <<"external_id">>]),
        Attrs = (hd(QueryStanza#xmlel.children))#xmlel.attrs,
        BrokenAttrs = proplists:delete(<<"node">>, Attrs),
        BrokenStanza =
        QueryStanza#xmlel{children =
                          (hd(QueryStanza#xmlel.children))#xmlel{attrs =
                                                             BrokenAttrs}},
        expect_iq_error(BrokenStanza, Alice)
    end).

malformed_user(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza = get_request(<<"468">>, ?ALICE_UUID,
                                  [<<"user">>, <<"email">>, <<"external_id">>]),
        Attrs = (hd(QueryStanza#xmlel.children))#xmlel.attrs,
        BrokenAttrs = [{<<"node">>, <<"baduserbad">>} |
                       proplists:delete(<<"node">>, Attrs)],
        BrokenStanza =
        QueryStanza#xmlel{children =
                          (hd(QueryStanza#xmlel.children))#xmlel{attrs =
                                                             BrokenAttrs}},
        expect_iq_error(BrokenStanza, Alice)
    end).

wrong_type(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza = get_request(<<"469">>, ?ALICE_UUID, []),
        BrokenStanza =
        QueryStanza#xmlel{attrs = lists:keyreplace(<<"type">>, 1,
                                                   QueryStanza#xmlel.attrs,
                                                   {<<"type">>, <<"fnord">>})},
        expect_iq_error(BrokenStanza, Alice)
    end).

missing_var(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        BrokenStanza =
        request_wrapper(<<"470">>,
                        <<"get">>, ?ALICE_UUID,
                        [#xmlel{name = <<"field">>}]),
        expect_iq_error(BrokenStanza, Alice)
    end).


non_existant_field(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza = get_request(<<"471">>, ?ALICE_UUID,
                                  [<<"user">>, <<"doesntexist">>]),
        expect_iq_error(QueryStanza, Alice)
    end).

wrong_type2(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza = invalid_request(<<"472">>, ?ALICE_UUID,
                                      [<<"user">>, <<"doesntexist">>]),
        expect_iq_error(QueryStanza, Alice)
    end).

garbage_get(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza = garbage_request(<<"473">>, ?ALICE_UUID, <<"get">>),
        % No get fields = get all, so this one still works:
        expect_iq_success(QueryStanza, Alice)
    end).

%%--------------------------------------------------------------------
%% mod_wocky_user 'set' tests
%%--------------------------------------------------------------------

set_fields(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza =
        set_request(<<"571">>, ?ALICE_UUID, set_fields()),
        expect_iq_success(QueryStanza, Alice),

        #{handle := <<"Alieee">>, first_name := <<"Bob">>} =
        wocky_db_user:find_user(?ALICE_UUID, ?LOCAL_CONTEXT)
    end).

set_other_user(Config) ->
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
        QueryStanza =
        set_request(<<"572">>, ?ALICE_UUID, set_fields()),
        expect_iq_error(QueryStanza, Bob)
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
        expect_iq_error(BrokenStanza, Alice)
    end).

delete_user(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza = delete_request(<<"999">>),
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
        set_request(<<"574">>, ?ALICE_UUID,
                    [{<<"phone_number">>, <<"string">>, <<"+1444">>}]),
        expect_iq_error(QueryStanza, Alice)
    end).

non_writable_field(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza =
        set_request(<<"575">>, ?ALICE_UUID,
                    [{<<"user">>, <<"uuid">>, wocky_db:create_id()}]),
        expect_iq_error(QueryStanza, Alice)
    end).

blank_handle(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza =
        set_request(<<"576">>, ?ALICE_UUID,
                    [{<<"handle">>, <<"string">>, <<"">>}]),
        expect_iq_error(QueryStanza, Alice)
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
        expect_iq_error(BrokenStanza, Alice)
    end).

set_wrong_type(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza =
        request_wrapper(<<"578">>, <<"set">>, ?ALICE_UUID,
                        [#xmlel{name = <<"field">>,
                                attrs = [{<<"var">>, <<"first_name">>},
                                         {<<"type">>, <<"strOng">>}]
                               }]),
        expect_iq_error(QueryStanza, Alice)
    end).

set_missing_var(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        BrokenStanza =
        request_wrapper(<<"579">>, <<"set">>, ?ALICE_UUID,
                        [#xmlel{name = <<"field">>}]),
        expect_iq_error(BrokenStanza, Alice)
    end).

set_missing_value(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        BrokenStanza =
        request_wrapper(<<"580">>, <<"set">>, ?ALICE_UUID,
                        [#xmlel{name = <<"field">>,
                                attrs = [{<<"var">>, <<"first_name">>},
                                         {<<"type">>, <<"string">>}],
                                children = [#xmlel{name = <<"other">>}]
                               }]),
        expect_iq_error(BrokenStanza, Alice)
    end).

handle_clash(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        QueryStanza =
        set_request(<<"581">>, ?ALICE_UUID, set_fields()),
        expect_iq_success(QueryStanza, Alice),
        BobQueryStanza =
        set_request(<<"582">>, ?BOB_UUID, set_fields()),
        expect_iq_error(BobQueryStanza, Bob),

        BobUUID = ?BOB_UUID,
        #{handle := BobUUID, first_name := null} =
        wocky_db_user:find_user(?BOB_UUID, ?LOCAL_CONTEXT)
    end).

same_handle(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza =
        set_request(<<"583">>, ?ALICE_UUID, set_fields()),
        expect_iq_success(QueryStanza, Alice)
    end).

garbage_set(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza =
        garbage_request(<<"584">>, ?ALICE_UUID, <<"set">>),
        % Successfully set nothing:
        expect_iq_success(QueryStanza, Alice)
    end).

invalid_email(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza =
        set_request(<<"585">>, ?ALICE_UUID,
                    [{<<"email">>, <<"string">>, <<"notanemail">>}]),
        expect_iq_error(QueryStanza, Alice)
    end).

invalid_avatar(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza =
        set_request(<<"586">>, ?ALICE_UUID,
                    [{<<"avatar">>, <<"file">>, <<"notaURL">>}]),
        expect_iq_error(QueryStanza, Alice)
    end).

non_local_avatar(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza =
        set_request(<<"587">>, ?ALICE_UUID,
                    [{<<"avatar">>, <<"file">>,
                      <<"tros:user@otherserver.com/file/",
                        ?AVATAR_FILE/binary>>}]),
        expect_iq_error(QueryStanza, Alice)
    end).

non_uuid_avatar(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        QueryStanza =
        set_request(<<"588">>, ?ALICE_UUID,
                    [{<<"avatar">>, <<"file">>,
                      <<"tros:", (?ALICE_UUID)/binary, "@",
                        ?LOCAL_CONTEXT/binary, "/file/blahblah">>}]),
        expect_iq_error(QueryStanza, Alice)
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
     {<<"first_name">>, <<"string">>, <<"Bob">>},
     {<<"email">>, <<"string">>, <<"bob@alice.com">>},
     {<<"avatar">>, <<"file">>,
      <<"tros:043e8c96-ba30-11e5-9912-ba0be0483c18@",
        ?LOCAL_CONTEXT/binary, "/file/", ?AVATAR_FILE/binary>>}].

delete_request(ID) ->
    #xmlel{name = <<"iq">>,
           attrs = [{<<"id">>, ID},
                    {<<"type">>, <<"set">>}],
           children = [#xmlel{name = <<"delete">>,
                              attrs = [{<<"xmlns">>,
                                        <<"hippware.com/hxep/user">>}]
                             }]}.
