%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
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

-module(ejabberd_login_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include_lib("exml/include/exml.hrl").

-include("wocky_db_seed.hrl").
-include("wocky.hrl").


%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, login},
     {group, login_scram},
     {group, login_scram_store_plain},
     {group, token_auth},
     {group, messages}].

groups() ->
    [{login, [no_sequence], login_tests()},
     {login_scram, [sequence], scram_tests()},
     {login_scram_store_plain, [sequence], scram_tests()},
     {token_auth, [sequence], [acquire_token, release_token, login_with_token]},
     {messages, [sequence], [messages_story]}].

login_tests() ->
    [log_one,
     log_non_existent_plain,
     log_one_digest,
     log_non_existent_digest,
     log_one_scram,
     log_non_existent_scram,
     blocked_user].

scram_tests() ->
    [log_one, log_one_scram].

suite() ->
    escalus:suite().


%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = test_helper:start_ejabberd(),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config),
    test_helper:stop_ejabberd().

init_per_group(GroupName, Config) when GroupName =:= login;
      GroupName =:= login_scram; GroupName =:= login_scram_store_plain ->
    config_password_format(GroupName),
    Config2 = escalus:create_users(Config, {by_name, [alice, bob]}),
    assert_password_format(GroupName, Config2);
init_per_group(_GroupName, Config) ->
    escalus:create_users(Config, {by_name, [alice, bob]}).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, {by_name, [alice, bob]}).

init_per_testcase(blocked_user, Config) ->
    Domain = ct:get_config(ejabberd_domain),
    escalus_ejabberd:rpc(acl, add, [Domain, blocked, {user, ?ALICE}]),
    escalus:init_per_testcase(blocked_user, Config);
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(blocked_user, Config) ->
    Domain = ct:get_config(ejabberd_domain),
    escalus_ejabberd:rpc(acl, delete, [Domain, blocked, {user, ?ALICE}]),
    Config;
end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).


%%--------------------------------------------------------------------
%% Test cases
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% login groups

log_one(Config) ->
    escalus:story(Config, [{alice, 1}], fun (Alice) ->
        escalus_client:send(Alice, escalus_stanza:chat_to(Alice, <<"Hi!">>)),
        escalus:assert(is_chat_message, [<<"Hi!">>],
                       escalus_client:wait_for_stanza(Alice))
    end).

log_one_digest(Config) ->
    log_one([{escalus_auth_method, <<"DIGEST-MD5">>} | Config]).

log_one_scram(Config) ->
    log_one([{escalus_auth_method, <<"SCRAM-SHA-1">>} | Config]).

log_non_existent_plain(Config) ->
    {auth_failed, _, _} = log_non_existent(Config).

log_non_existent_digest(Config) ->
    R = log_non_existent([{escalus_auth_method, <<"DIGEST-MD5">>} | Config]),
    {expected_challenge, _, _} = R.

log_non_existent_scram(Config) ->
    R = log_non_existent([{escalus_auth_method, <<"SCRAM-SHA-1">>} | Config]),
    {expected_challenge, _, _} = R.

log_non_existent(Config) ->
    [{karen, UserSpec}] = escalus_users:get_users({by_name, [karen]}),
    {error, {connection_step_failed, _, R}} =
        escalus_client:start(Config, UserSpec, <<"res">>),
    R.

blocked_user(_Config) ->
    [{_, Spec}] = escalus_users:get_users({by_name, [alice]}),
    try
        {ok, _Alice, _Spec2, _Features} = escalus_connection:start(Spec),
        ct:fail("Alice authenticated but shouldn't")
    catch
        error:{assertion_failed, assert, is_iq_result, Stanza, _Bin} ->
            <<"cancel">> = exml_query:path(Stanza, [{element, <<"error">>},
                                                    {attr, <<"type">>}])
    end,
    ok.


%%--------------------------------------------------------------------
%% token group

acquire_token(Config) ->
    escalus:story(Config, [{alice, 1}], fun (Alice) ->
        escalus_client:send(Alice, token_stanza(<<"get">>)),
        Reply = escalus_client:wait_for_stanza(Alice),
        <<"result">> = exml_query:path(Reply, [{attr, <<"type">>}]),
        <<"$T$", _/binary>> =
            exml_query:path(Reply, [{element, <<"query">>}, cdata])
    end).

release_token(Config) ->
    escalus:story(Config, [{alice, 1}], fun (Alice) ->
        escalus_client:send(Alice, token_stanza(<<"set">>)),
        Reply = escalus_client:wait_for_stanza(Alice),
        <<"result">> = exml_query:path(Reply, [{attr, <<"type">>}])
    end).

login_with_token(Config) ->
    Domain = ct:get_config(ejabberd_domain),
    {ok, Token} = escalus_ejabberd:rpc(wocky_db_user, assign_token,
                                       [?ALICE, Domain, <<"res1">>]),
    Config2 = escalus_users:update_userspec(Config, alice, password, Token),
    log_one(Config2).


%%--------------------------------------------------------------------
%% message group

messages_story(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun (Alice, Bob) ->
        % Alice sends a message to Bob
        escalus_client:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi!">>)),

        % Bob gets the message
        escalus_assert:is_chat_message(
          <<"Hi!">>, escalus_client:wait_for_stanza(Bob))
    end).


%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

set_store_password(Type) ->
    XMPPDomain = escalus_ejabberd:unify_str_arg(ct:get_config(ejabberd_domain)),
    AuthOpts = escalus_ejabberd:rpc(ejabberd_config, get_local_option,
                                    [{auth_opts, XMPPDomain}]),
    NewAuthOpts = lists:keystore(password_format, 1, AuthOpts,
                                 {password_format, Type}),
    escalus_ejabberd:rpc(ejabberd_config, add_local_option,
                         [{auth_opts, XMPPDomain}, NewAuthOpts]).

config_password_format(login_scram) ->
    set_store_password(scram);
config_password_format(_) ->
    set_store_password(plain).

assert_password_format(GroupName, Config) ->
    Users = proplists:get_value(escalus_users, Config),
    [verify_format(GroupName, User) || User <- Users],
    Config.

verify_format(GroupName, {_User, Props}) ->
    Username = escalus_utils:jid_to_lower(proplists:get_value(username, Props)),
    Server = proplists:get_value(server, Props),
    Password = proplists:get_value(password, Props),

    SPassword = escalus_ejabberd:rpc(ejabberd_auth, get_password,
                                     [Username, Server]),
    do_verify_format(GroupName, Password, SPassword).

do_verify_format(login_scram, _Password, SPassword) ->
    %% returned password is a tuple containing scram data
    {_, _, _, _} = SPassword;
do_verify_format(_, Password, SPassword) ->
    Password = SPassword.

token_stanza(Type) ->
    escalus_stanza:iq(Type, escalus_stanza:query_el(?NS_TOKEN, [])).
