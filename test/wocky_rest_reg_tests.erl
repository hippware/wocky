%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for wocky_reg_tests.erl
-module(wocky_rest_reg_tests).

-include_lib("eunit/include/eunit.hrl").
-include("wocky_db_seed.hrl").

-import(wocky_rest_reg,
        [verify_session/2]).

wocky_rest_reg_test_() -> {
  "wocky_rest_reg",
  setup, fun before_all/0, fun after_all/1,
  [
    test_verify_session()
  ]}.

before_all() ->
    ok = wocky_app:start(),
    ok = wocky_db_seed:prepare_tables(shared, [handle_to_user,
                                               phone_number_to_user]),
    ok = wocky_db_seed:prepare_tables(?LOCAL_CONTEXT, [user, auth_token]),
    ok = wocky_db_seed:seed_tables(shared, [handle_to_user,
                                            phone_number_to_user]),
    ok = wocky_db_seed:seed_tables(?LOCAL_CONTEXT, [user, auth_token]),
    ok.

after_all(_) ->
    ok = wocky_app:stop(),
    ok.

test_verify_session() ->
    BaseFields = #{server => ?LOCAL_CONTEXT,
                   resource => ?RESOURCE},
    Fields = BaseFields#{uuid => ?ALICE},

    { "verify_session", [
       { "accepts a valid session/user combination", [
         ?_assert(verify_session(Fields, ?TOKEN))
       ]},
       { "rejects an invalid session/user combination", [
         ?_assertNot(verify_session(Fields, <<"$T$invalidToken">>))
       ]},
       { "maps auth_user to the right uuid", [
         ?_assert(verify_session(BaseFields#{userID => ?AUTH_USER},
                                 ?TOKEN)),
         ?_assertNot(verify_session(BaseFields#{userID => ?AUTH_USER},
                                    <<"$T$badtoken">>))
       ]}
    ]}.
