-module(escalus_wocky).

-behaviour(escalus_user_db).

%% escalus_user_db callbacks
-export([start/1,
         stop/1,
         create_users/2,
         delete_users/2]).

%%--------------------------------------------------------------------
%% escalus_user_db callbacks
%%--------------------------------------------------------------------

start(_) -> ok.
stop(_) -> ok.

% -spec create_users(escalus:config(), [user_spec()]) -> escalus:config().
create_users(Config, Users) ->
    lists:foreach(fun({_Name, UserSpec}) ->
                          register_user(Config, UserSpec)
                  end, Users),
    lists:keystore(escalus_users, 1, Config, {escalus_users, Users}).

% -spec delete_users(escalus:config(), [user_spec()]) -> escalus:config().
delete_users(Config, Users) ->
    lists:foreach(fun({_Name, UserSpec}) ->
                          unregister_user(Config, UserSpec)
                  end, Users),
    Config.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

register_user(Config, UserSpec) ->
    [U, S, P] = escalus_users:get_usp(Config, UserSpec),
    ejabberd_auth_wocky:try_register(U, S, P).

unregister_user(Config, UserSpec) ->
    [U, S, _P] = escalus_users:get_usp(Config, UserSpec),
    ejabberd_auth_wocky:remove_user(U, S).
