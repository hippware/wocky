%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for wocky_db_user.erl
-module(wocky_db_user_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(DOMAIN, <<"localhost">>).
-define(USER, <<"bob">>).
-define(PASS, <<"password">>).

%%
%% Suite configuration
%%

all() -> [
  does_user_exist_returns_existence,
  create_user_creates_user,
  create_user_fails_if_user_exists,
  get_password_gets_password,
  get_password_fails_if_user_dne,
  set_password_sets_password,
  set_password_fails_if_user_dne,
  remove_user_removes_user,
  remove_user_succeeds_if_user_dne
].

suite() -> [
  {timetrap, {seconds, 30}}
].


%%
%% Setup/Teardown
%%

init_per_suite(Config) ->
  ok = wocky_app:start(),
  Config.

end_per_suite(_Config) ->
  ok = wocky_app:stop(),
  ok.

init_per_testcase(_TestCase, Config) ->
  UUID = ossp_uuid:make(v1, binary),
  Query1 = "INSERT INTO username_to_user (id, domain, username) VALUES (?, ?, ?)",
  {ok, _} = wocky_db:pquery(shared, Query1, [UUID, ?DOMAIN, ?USER], quorum),

  Query2 = "INSERT INTO user (id, domain, username, password) VALUES (?, ?, ?, ?)",
  {ok, _} = wocky_db:pquery(?DOMAIN, Query2, [UUID, ?DOMAIN, ?USER, ?PASS], quorum),
  Config.

end_per_testcase(_TestCase, _Config) ->
  {ok, _} = wocky_db:pquery(shared, <<"TRUNCATE username_to_user">>, [], quorum),
  {ok, _} = wocky_db:pquery(?DOMAIN, <<"TRUNCATE user">>, [], quorum),
  ok.


%%
%% Testcases
%%

does_user_exist_returns_existence(_Config) ->
  true  = wocky_db_user:does_user_exist(?DOMAIN, ?USER),
  false = wocky_db_user:does_user_exist(?DOMAIN, <<"baduser">>),
  ok.

create_user_creates_user(_Config) ->
  ok    = wocky_db_user:create_user(?DOMAIN, <<"alice">>, ?PASS),
  true  = wocky_db_user:does_user_exist(?DOMAIN, <<"alice">>),
  ok.

create_user_fails_if_user_exists(_Config) ->
  {error, exists} = wocky_db_user:create_user(?DOMAIN, ?USER, ?PASS),
  ok.

get_password_gets_password(_Config) ->
  ?PASS = wocky_db_user:get_password(?DOMAIN, ?USER),
  ok.

get_password_fails_if_user_dne(_Config) ->
  {error, not_found} = wocky_db_user:get_password(?DOMAIN, <<"baduser">>),
  ok.

set_password_sets_password(_Config) ->
  ok = wocky_db_user:set_password(?DOMAIN, ?USER, <<"newpass">>),
  <<"newpass">> = wocky_db_user:get_password(?DOMAIN, ?USER),
  ok.

set_password_fails_if_user_dne(_Config) ->
  {error, not_found} = wocky_db_user:set_password(?DOMAIN, <<"baduser">>, ?PASS),
  ok.

remove_user_removes_user(_Config) ->
  ok    = wocky_db_user:remove_user(?DOMAIN, ?USER),
  false = wocky_db_user:does_user_exist(?DOMAIN, ?USER),
  ok.

remove_user_succeeds_if_user_dne(_Config) ->
  ok = wocky_db_user:remove_user(?DOMAIN, <<"baduser">>),
  ok.
