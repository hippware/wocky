-module(reg_SUITE).

-export([
         suite/0,
         all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

-export([
         incorrect_req_type/1,
         missing_fields/1,
         unauthorized_new/1,
         new_user/1,
         unauthorized_update/1,
         update/1,
         invalid_phone_number/1,
         session_id_update/1,
         session_id_update_phone_number/1
        ]).

-include("wocky_db_seed.hrl").

-define(URL, "http://localhost:1096").
-define(TEST_HANDLE, <<"TinyRooooobot">>).

all() -> [{group, reg}].

groups() ->
    [{reg, [sequence], reg_cases()}].

reg_cases() ->
    [
     incorrect_req_type,
     missing_fields,
     unauthorized_new,
     new_user,
     unauthorized_update,
     update,
     invalid_phone_number,
     session_id_update,
     session_id_update_phone_number
%     large_request,
    ].

suite() ->
    escalus:suite().

init_per_suite(Config) ->
    test_helper:start_ejabberd(),
    ok = wocky_db_seed:prepare_tables(?LOCAL_CONTEXT, [user, auth_token]),
    ok = wocky_db_seed:prepare_tables(shared, [handle_to_user,
                                               phone_number_to_user]),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config),
    test_helper:stop_ejabberd(),
    ok.

init_per_group(_Group, ConfigIn) -> ConfigIn.

end_per_group(_Group, Config) -> Config.

init_per_testcase(_CaseName, Config) ->
    wocky_db_seed:clear_tables(?LOCAL_CONTEXT, [user, auth_token]),
    wocky_db_seed:clear_tables(shared, [handle_to_user,
                                        phone_number_to_user]),
    Config.

end_per_testcase(_CaseName, Config) ->
    Config.

%%%===================================================================
%%% Tests
%%%===================================================================

incorrect_req_type(_) ->
    {ok, {405, _}} = httpc:request(get, {?URL, []}, [], [{full_result, false}]).

missing_fields(_) ->
    JSON2 = encode(proplists:delete(resource, test_data())),
    {ok, {400, _}} = request(JSON2).

unauthorized_new(_) ->
    start_digits_server(false),
    JSON = encode(test_data()),
    {ok, {401, _}} = request(JSON),
    stop_digits_server().

new_user(_) ->
    start_digits_server(true),
    JSON = encode(test_data()),
    {ok, {201, Body}} = request(JSON),
    verify_new_result(Body),
    stop_digits_server().

unauthorized_update(_) ->
    start_digits_server(false),
    User = create_user(),
    verify_handle(User, ?TEST_HANDLE),
    Data = lists:keyreplace(handle, 1, test_data(), {handle, <<"NewHandle">>}),
    JSON = encode(Data),
    {ok, {401, _}} = request(JSON),
    verify_handle(User, ?TEST_HANDLE),
    stop_digits_server().

update(_) ->
    start_digits_server(true),
    User = create_user(),
    Data = lists:keyreplace(handle, 1, test_data(), {handle, <<"NewHandle">>}),
    JSON = encode(Data),
    {ok, {201, _Body}} = request(JSON),
    verify_handle(User, <<"NewHandle">>),
    stop_digits_server().

invalid_phone_number(_) ->
    start_digits_server(true),
    User = create_user(),
    Data = lists:keyreplace(phoneNumber, 1, test_data(),
                            {phoneNumber, <<"+5551231234">>}),
    JSON = encode(Data),
    {ok, {401, _Body}} = request(JSON),
    verify_phone_number(User, ?PHONE_NUMBER),
    stop_digits_server().

session_id_update(_) ->
    start_digits_server(true),
    User = create_user(),
    JSON = encode(test_data()),
    {ok, {201, Body}} = request(JSON),
    {struct, Elements} = mochijson2:decode(Body),
    SessionID = proplists:get_value(<<"sessionID">>, Elements),
    UUID = proplists:get_value(<<"uuid">>, Elements),
    Data = lists:keyreplace(handle, 1, session_test_data(UUID, SessionID),
                            {handle, <<"NewHandle">>}),
    {ok, {201, Body2}} = request(encode(Data)),
    {struct, Elements2} = mochijson2:decode(Body2),
    true = proplists:get_value(<<"handleSet">>, Elements2),
    <<"NewHandle">> = proplists:get_value(<<"handle">>, Elements2),
    verify_handle(User, <<"NewHandle">>),
    stop_digits_server().


session_id_update_phone_number(_) ->
    start_digits_server(true),
    User = create_user(),
    JSON = encode(test_data()),
    {ok, {201, Body}} = request(JSON),
    {struct, Elements} = mochijson2:decode(Body),
    SessionID = proplists:get_value(<<"sessionID">>, Elements),
    UUID = proplists:get_value(<<"uuid">>, Elements),
    Data = lists:keyreplace(phoneNumber, 1, session_test_data(UUID, SessionID),
                            {phoneNumber, <<"+5551231234">>}),
    {ok, {201, Body2}} = request(encode(Data)),
    {struct, Elements2} = mochijson2:decode(Body2),
    false = proplists:get_value(<<"phoneNumberSet">>, Elements2),
    ?PHONE_NUMBER = proplists:get_value(<<"phoneNumber">>, Elements2),
    verify_phone_number(User, ?PHONE_NUMBER),
    stop_digits_server().

%%%===================================================================
%%% Helpers
%%%===================================================================

start_digits_server(Result) ->
    fake_digits_server:start(Result).
stop_digits_server() ->
    fake_digits_server:stop().

encode(Data) ->
    iolist_to_binary(mochijson2:encode({struct, Data})).

request(Body) ->
    httpc:request(post, {?URL, [{"Accept", "application/json"}], "application/json", Body},
                  [], [{full_result, false}]).

test_data() ->
    [{handle, ?TEST_HANDLE},
     {resource, <<"Kurita">>},
     {emailAddress, <<"me@alice.com">>},
     {userID, <<"701990807448920064">>},
     {phoneNumber, ?PHONE_NUMBER},
     {'X-Auth-Service-Provider', list_to_binary(fake_digits_server:url())},
     {'X-Verify-Credentials-Authorization',
        <<"OAuth oauth_signature=\"32%2Bf2UY9txk%2BIcV0mV7P55R9%2Fkw%3D\","
        "oauth_nonce=\"EF4F602C-0BBF-48F1-91C7-7CD344E7D0A1\","
        "oauth_timestamp=\"1456795178\","
        "oauth_consumer_key=\"e527IQiWSXZ5WHNxROUZk87uV\","
        "oauth_token=\"701990807448920064-JxNX4i57y5Wp6xBDVjNwKB4ZYUcC8FJ\","
        "oauth_version=\"1.0\",oauth_signature_method=\"HMAC-SHA1\"">>},
     {firstName, <<"Alice">>},
     {lastName, <<"Alison">>}
    ].

session_test_data(UUID, SessionID) ->
    [{handle, ?TEST_HANDLE},
     {uuid, UUID},
     {sessionID, SessionID},
     {resource, <<"Kurita">>},
     {emailAddress, <<"me@alice.com">>},
     {phoneNumber, ?PHONE_NUMBER},
     {firstName, <<"Alice">>},
     {lastName, <<"Alison">>}
    ].

verify_new_result(Body) ->
    {struct, Elements} = mochijson2:decode(Body),
    verify_elements(maps:from_list(Elements)).

verify_elements(#{
  <<"handle">> := ?TEST_HANDLE,
  <<"resource">> := <<"Kurita">>,
  <<"emailAddress">> := <<"me@alice.com">>,
  <<"userID">> := <<"701990807448920064">>,
  <<"phoneNumber">> := ?PHONE_NUMBER,
  <<"firstName">> := <<"Alice">>,
  <<"lastName">> := <<"Alison">>,
  <<"uuid">> := _,
  <<"sessionID">> := _,
  <<"isNew">> := true,
  <<"phoneNumberSet">> := true,
  <<"handleSet">> := true
 }) -> ok;
verify_elements(Fields) ->
    ct:fail("Missing or incorrect fields: ~p", [Fields]).

create_user() ->
    User = wocky_db_user:create_id(),
    Fields = #{
      user => User,
      server => ?LOCAL_CONTEXT,
      handle => ?TEST_HANDLE,
      email => <<"me@alice.com">>,
      auth_user => <<"701990807448920064">>,
      phone_number => ?PHONE_NUMBER,
      first_name => <<"Alice">>,
      last_name => <<"Alison">>
     },
    wocky_db_user:create_user(Fields),
    true = wocky_db_user:maybe_set_handle(User, ?LOCAL_CONTEXT, ?TEST_HANDLE),
    true = wocky_db_user:set_phone_number(User, ?LOCAL_CONTEXT,
                                                ?PHONE_NUMBER),
    User.

verify_handle(User, Handle) ->
    {User, ?LOCAL_CONTEXT} = wocky_db_user:get_by_handle(Handle),
    Handle = wocky_db_user:get_handle(User, ?LOCAL_CONTEXT).

verify_phone_number(User, PhoneNumber) ->
    {User, ?LOCAL_CONTEXT} = wocky_db_user:get_by_phone_number(PhoneNumber),
    PhoneNumber = wocky_db_user:get_phone_number(User, ?LOCAL_CONTEXT).
