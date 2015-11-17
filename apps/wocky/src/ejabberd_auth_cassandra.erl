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
%%% {auth_method, cassandra}.
%%% '''

-module(ejabberd_auth_cassandra).

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

-spec start(Host :: ejabberd:server()) -> ok.
start(_Host) ->
    ok.


-spec stop(Host :: ejabberd:server()) -> ok.
stop(_Host) ->
    ok.


-spec store_type(Host :: ejabberd:lserver()) -> scram | plain | external.
store_type(Host) ->
    case scram:enabled(Host) of
        false -> plain;
        true -> scram
    end.


-spec login(User :: ejabberd:luser(),
            Server :: ejabberd:lserver()) -> boolean().
login(_User, _Server) ->
    erlang:error(not_implemented).


-spec set_password(User :: ejabberd:luser(),
                   Server :: ejabberd:lserver(),
                   Password :: binary()
                  ) -> ok | {error, not_allowed | invalid_jid}.
set_password(User, Server, Password) ->
    PreparedPass = prepare_password(Server, Password),
    case wocky_user:set_password(Server, User, PreparedPass) of
        ok ->
            ok;

        {error, not_found} ->
            {error, invalid_jid}
    end.


-spec check_password(User :: ejabberd:luser(),
                     Server :: ejabberd:lserver(),
                     Password :: binary()) -> boolean().
check_password(User, Server, Password) ->
    case wocky_user:get_password(Server, User) of
        StoredPassword when is_binary(StoredPassword) ->
            case scram:deserialize(StoredPassword) of
                #scram{} = Scram ->
                    scram:check_password(Password, Scram);

                {error, _}->
                    %% Not a SCRAM password
                    Password == StoredPassword
                end;

        {error, _} ->
            false
    end.


-spec check_password(User :: ejabberd:luser(),
                     Server :: ejabberd:lserver(),
                     Password :: binary(),
                     Digest :: binary(),
                     DigestGen :: fun()) -> boolean().
check_password(User, Server, Password, Digest, DigestGen) ->
    case wocky_user:get_password(Server, User) of
        StoredPassword when is_binary(StoredPassword) ->
            case scram:deserialize(StoredPassword) of
                #scram{} = Scram ->
                    scram:check_digest(Scram, Digest, DigestGen, Password);

                {error, _}->
                    %% Not a SCRAM password
                    Password == StoredPassword
                end;

        {error, _} ->
            false
    end.


%% Not really suitable for use since it does not pass in extra profile information.
%% Exists for completeness more than anything else (at the moment)
-spec try_register(User :: ejabberd:luser(),
                   Server :: ejabberd:lserver(),
                   Password :: binary()
                  ) -> ok | {error, exists | not_allowed | term()}.
try_register(User, Server, Password) ->
    PreparedPass = prepare_password(Server, Password),
    wocky_user:create_user(Server, User, PreparedPass).


-spec dirty_get_registered_users() -> [ejabberd:simple_bare_jid()].
dirty_get_registered_users() ->
    Servers = ejabberd_config:get_vh_by_auth_method(cassandra),
    lists:flatmap(
        fun(Server) ->
            get_vh_registered_users(Server)
        end, Servers).


%% This call is costly and infrequently used. Stub it out.
-spec get_vh_registered_users(Server :: ejabberd:lserver()) -> [ejabberd:simple_bare_jid()].
get_vh_registered_users(_Server) ->
    [].

-spec get_vh_registered_users(Server :: ejabberd:lserver(),
                              Opts :: list()
                             ) -> [ejabberd:simple_bare_jid()].
get_vh_registered_users(Server, _Opts) ->
    get_vh_registered_users(Server).


-spec get_vh_registered_users_number(Server :: ejabberd:lserver()) -> integer().
get_vh_registered_users_number(Server) ->
    length(get_vh_registered_users(Server)).


-spec get_vh_registered_users_number(Server :: ejabberd:lserver(),
                                     Opts :: list()) -> integer().
get_vh_registered_users_number(Server, _Opts) ->
    get_vh_registered_users_number(Server).


-spec get_password(User :: ejabberd:luser(),
                   Server :: ejabberd:lserver()
                  ) -> scram:scram_tuple() | binary() | false.
get_password(User, Server) ->
    case wocky_user:get_password(Server, User) of
        StoredPassword when is_binary(StoredPassword) ->
            case scram:deserialize(StoredPassword) of
                #scram{} = Scram ->
                    scram:scram_to_tuple(Scram);

                {error, _}->
                    %% Not a SCRAM password
                    StoredPassword
            end;

        {error, _} ->
            false
    end.


-spec get_password_s(User :: ejabberd:luser(),
                     Server :: ejabberd:lserver()) -> binary().
get_password_s(User, Server) ->
    case get_password(User, Server) of
        Password when is_binary(Password) ->
            Password;

        _ ->
            <<"">>
    end.


-spec get_password(User :: ejabberd:luser(),
                   Server :: ejabberd:lserver(),
                   DefaultValue :: binary()
                  ) -> scram:scram_tuple() | binary() | false.
get_password(User, Server, _DefaultValue) ->
    get_password(User, Server).


-spec does_user_exist(User :: ejabberd:luser(),
                      Server :: ejabberd:lserver()
                     ) -> boolean() | {error, atom()}.
does_user_exist(User, Server) ->
    wocky_user:does_user_exist(Server, User).


-spec remove_user(User :: ejabberd:luser(),
                  Server :: ejabberd:lserver()
                 ) -> ok | {error, not_allowed}.
remove_user(User, Server) ->
    case does_user_exist(User, Server) of
        true ->
            wocky_user:remove_user(Server, User);

        false ->
            %% Mission accomplished, the user no longer exists
            ok
    end.


%% @doc Remove user if the provided password is correct.
%% This functionality is unused (only called from mod_register) so just stub it out.
-spec remove_user(User :: ejabberd:luser(),
                  Server :: ejabberd:lserver(),
                  Password :: binary()
                 ) -> ok | {error, not_exists | not_allowed | bad_request}.
remove_user(User, Server, Password) ->
    case does_user_exist(User, Server) of
        true ->
          case check_password(User, Server, Password) of
              true -> wocky_user:remove_user(Server, User);
              false -> {error, not_allowed}
          end;

        false ->
            {error, not_exists}
    end.


-spec plain_password_required() -> boolean().
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
