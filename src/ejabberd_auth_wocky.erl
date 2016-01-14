%%% @copyright 2015+ Hippware, Inc.
%%% @doc Cassandra-backed authentication backend
%%%
%%% This module serves a number of purposes:
%%%
%%% 1) Pluggable ejabberd authentication backend ({@link ejabberd_gen_auth})
%%% 2) Application specific user functionality
%%%
%%% Complications arise because they are not identical.
%%%
%%% Ejabberd users are a (localpart, domainpart) pair with localpart being the
%%% 'username'. Wocky users are a (localpart, domainpart, username) tuple with
%%% username being a separate quantity (which is also globally unique across all
%%% domains).
%%%
%%% In order to utilise existing code, this module needs to conform to {@link
%%% ejabberd_gen_auth} but not all of the functions required of (1) make sense
%%% for (2). Hence, for those functions which don't make sense, a "best effort"
%%% implementation which is "least surprising" will have to suffice. In other
%%% words, all the functions of (1) need to be implemented, but not all of them
%%% will be useful or are expected to be used in normal operations.
%%%
%%%
%%% For schema, see priv/schema*.cql
%%%
%%% Enable with the following in ejabberd.cfg
%%%
%%% ```
%%% {auth_method, wocky}.
%%% '''

-module(ejabberd_auth_wocky).

-behaviour(ejabberd_gen_auth).
-export([start/1,
         stop/1,
         store_type/1,
         login/2,
         set_password/3,
         check_password/3,
         check_password/5,
         try_register/3,
         dirty_get_registered_users/0,
         get_vh_registered_users/1,
         get_vh_registered_users/2,
         get_vh_registered_users_number/1,
         get_vh_registered_users_number/2,
         get_password/2,
         get_password_s/2,
         get_password/3,
         does_user_exist/2,
         remove_user/2,
         remove_user/3,
         plain_password_required/0]).

-include_lib("ejabberd/include/ejabberd.hrl").


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec start(ejabberd:server()) -> ok.
start(_Host) ->
    ok.


-spec stop(ejabberd:server()) -> ok.
stop(_Host) ->
    ok.


-spec store_type(ejabberd:lserver()) -> scram | plain.
store_type(LServer) ->
    case scram:enabled(LServer) of
        false -> plain;
        true -> scram
    end.


-spec login(ejabberd:luser(), ejabberd:lserver()) -> no_return().
login(_LUser, _LServer) ->
    erlang:error(not_implemented).


-spec set_password(ejabberd:luser(), ejabberd:lserver(), binary())
                  -> ok | {error, not_allowed | invalid_jid}.
set_password(LUser, LServer, Password) ->
    PreparedPass = prepare_password(LServer, Password),
    case wocky_db_user:set_password(LUser, LServer, PreparedPass) of
        ok ->
            ok;

        {error, not_found} ->
            {error, invalid_jid}
    end.


-spec check_password(ejabberd:luser(), ejabberd:lserver(), binary())
                    -> boolean().
check_password(LUser, LServer, Password) ->
    case wocky_db_user:get_password(LUser, LServer) of
        StoredPassword when is_binary(StoredPassword) ->
            case scram:deserialize(StoredPassword) of
                {ok, #scram{} = Scram} ->
                    scram:check_password(Password, Scram);

                {error, _}->
                    %% Not a SCRAM password
                    Password == StoredPassword
                end;

        {error, _} ->
            false
    end.


-spec check_password(ejabberd:luser(), ejabberd:lserver(), binary(), binary(),
                     fun()) -> boolean().
check_password(LUser, LServer, Password, Digest, DigestGen) ->
    case wocky_db_user:get_password(LUser, LServer) of
        StoredPassword when is_binary(StoredPassword) ->
            case scram:deserialize(StoredPassword) of
                {ok, #scram{} = Scram} ->
                    %% For the record, this function doesn't make any sense
                    %% when SCRAM is enabled. It just doesn't work. However,
                    %% this is how the other auth modules implement it, so that
                    %% is what we are going to do.
                    scram:check_digest(Scram, Digest, DigestGen, Password);

                {error, _}->
                    %% Not a SCRAM password
                    ejabberd_auth:check_digest(Digest, DigestGen,
                                               Password, StoredPassword)
                end;

        {error, _} ->
            false
    end.


%% Not really suitable for use since it does not pass in extra profile
%% information and we expect LUser to be a timeuuid. It is implemented
%% here to enable Escalus to create users in integration tests.
-spec try_register(ejabberd:luser(), ejabberd:lserver(), binary())
                  -> ok | {error, exists | not_allowed | term()}.
try_register(LUser, LServer, Password) ->
    PreparedPass = prepare_password(LServer, Password),
    wocky_db_user:create_user(LUser, LServer, LUser, PreparedPass).


-spec dirty_get_registered_users() -> [ejabberd:simple_bare_jid()].
dirty_get_registered_users() ->
    Servers = ejabberd_config:get_vh_by_auth_method(wocky),
    lists:flatmap(
        fun(Server) ->
            get_vh_registered_users(Server)
        end, Servers).


%% This call is costly and infrequently used. Stub it out.
-spec get_vh_registered_users(ejabberd:lserver())
                             -> [ejabberd:simple_bare_jid()].
get_vh_registered_users(_LServer) ->
    [].

-spec get_vh_registered_users(ejabberd:lserver(), list())
                             -> [ejabberd:simple_bare_jid()].
get_vh_registered_users(LServer, _Opts) ->
    get_vh_registered_users(LServer).


-spec get_vh_registered_users_number(ejabberd:lserver()) -> non_neg_integer().
get_vh_registered_users_number(LServer) ->
    length(get_vh_registered_users(LServer)).


-spec get_vh_registered_users_number(ejabberd:lserver(), list())
                                    -> non_neg_integer().
get_vh_registered_users_number(LServer, _Opts) ->
    get_vh_registered_users_number(LServer).


-spec get_password(ejabberd:luser(), ejabberd:lserver())
                  -> scram:scram_tuple() | binary() | false.
get_password(LUser, LServer) ->
    case wocky_db_user:get_password(LUser, LServer) of
        StoredPassword when is_binary(StoredPassword) ->
            case scram:deserialize(StoredPassword) of
                {ok, #scram{} = Scram} ->
                    scram:scram_to_tuple(Scram);

                {error, _}->
                    %% Not a SCRAM password
                    StoredPassword
            end;

        {error, _} ->
            false
    end.


-spec get_password_s(ejabberd:luser(), ejabberd:lserver()) -> binary().
get_password_s(LUser, LServer) ->
    case get_password(LUser, LServer) of
        Password when is_binary(Password) ->
            Password;

        _ ->
            <<"">>
    end.


-spec get_password(ejabberd:luser(), ejabberd:lserver(), binary())
                  -> scram:scram_tuple() | binary() | false.
get_password(LUser, LServer, _DefaultValue) ->
    get_password(LUser, LServer).


-spec does_user_exist(ejabberd:luser(), ejabberd:lserver()) -> boolean().
does_user_exist(LUser, LServer) ->
    wocky_db_user:does_user_exist(LUser, LServer).


-spec remove_user(ejabberd:luser(), ejabberd:lserver()) -> ok.
remove_user(LUser, LServer) ->
    case does_user_exist(LUser, LServer) of
        true ->
            wocky_db_user:remove_user(LUser, LServer);

        false ->
            %% Mission accomplished, the user no longer exists
            ok
    end.


%% @doc Remove user if the provided password is correct.
%% This functionality is unused (only called from mod_register)
%% so just stub it out.
-spec remove_user(ejabberd:luser(), ejabberd:lserver(), binary())
                 -> ok | {error, not_exists | not_allowed | bad_request}.
remove_user(LUser, LServer, Password) ->
    case does_user_exist(LUser, LServer) of
        true ->
          case check_password(LUser, LServer, Password) of
              true -> wocky_db_user:remove_user(LUser, LServer);
              false -> {error, not_allowed}
          end;

        false ->
            {error, not_exists}
    end.


-spec plain_password_required() -> false.
plain_password_required() ->
    false.


%%%===================================================================
%%% Internal functions
%%%===================================================================

prepare_password(Server, Password) ->
    case scram:enabled(Server) of
        true ->
            Scram = scram:password_to_scram(Password, scram:iterations(Server)),
            scram:serialize(Scram);

        false ->
            Password
    end.
