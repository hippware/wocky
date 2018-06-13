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
%%% Enable with the following in ejabberd.cfg
%%%
%%% ```
%%% {auth_method, wocky}.
%%% '''

-module(ejabberd_auth_wocky).

-compile({parse_transform, fun_chain}).

-include("wocky.hrl").

-behaviour(ejabberd_gen_auth).
-export([start/1,
         stop/1,
         store_type/1,
         authorize/1,
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
         does_user_exist/2,
         remove_user/2,
         remove_user/3]).


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec start(ejabberd:server()) -> ok.
start(Host) ->
    ejabberd_auth_odbc:start(Host).

-spec stop(ejabberd:server()) -> ok.
stop(Host) ->
    ejabberd_auth_odbc:stop(Host).

-spec store_type(ejabberd:lserver()) -> scram | plain.
store_type(LServer) ->
    ejabberd_auth_odbc:store_type(LServer).

-spec authorize(mongoose_credentials:t()) ->
    {ok, mongoose_credentials:t()} | {error, any()}.
authorize(Creds) ->
    ejabberd_auth:authorize_with_check_password(?MODULE, Creds).

-spec set_password(ejabberd:luser(), ejabberd:lserver(), binary()) ->
    ok | {error, not_allowed | invalid_jid}.
set_password(LUser, LServer, Password) ->
    ejabberd_auth_odbc:set_password(LUser, LServer, Password).

-spec check_password(ejabberd:luser(), ejabberd:lserver(), binary()) ->
    boolean().
check_password(LUser, _LServer, <<"$T$", _/binary>> = Token) ->
    case ?wocky_account:authenticate(token, {LUser, Token}) of
        {ok, _} -> true;
        _ -> false
    end;
check_password(LUser, LServer, Password) ->
    ejabberd_auth_odbc:check_password(LUser, LServer, Password).

-spec check_password(ejabberd:luser(), ejabberd:lserver(), binary(), binary(),
                     fun()) -> boolean().
check_password(LUser, LServer, Password, Digest, DigestGen) ->
    ejabberd_auth_odbc:check_password(LUser, LServer, Password,
                                      Digest, DigestGen).

%% Not really suitable for use since it does not pass in extra profile
%% information and we expect LUser to be a timeuuid. It is implemented
%% here to enable Escalus to create users in integration tests.
-spec try_register(ejabberd:luser(), ejabberd:lserver(), binary()) ->
    ok | {error, term()}.
try_register(LUser, LServer, Password) ->
    Username = mongoose_rdbms:escape(LUser),
    {Pwd, Details} = prepare_password(LServer, Password),
    ?wocky_account:register(Username, Pwd, Details).

-spec dirty_get_registered_users() -> [ejabberd:simple_bare_jid()].
dirty_get_registered_users() ->
    ejabberd_auth_odbc:dirty_get_registered_users().

-spec get_vh_registered_users(ejabberd:lserver()) ->
    [ejabberd:simple_bare_jid()].
get_vh_registered_users(LServer) ->
    ejabberd_auth_odbc:get_vh_registered_users(LServer).

-spec get_vh_registered_users(ejabberd:lserver(), list()) ->
    [ejabberd:simple_bare_jid()].
get_vh_registered_users(LServer, Opts) ->
    ejabberd_auth_odbc:get_vh_registered_users(LServer, Opts).

-spec get_vh_registered_users_number(ejabberd:lserver()) -> non_neg_integer().
get_vh_registered_users_number(LServer) ->
    ejabberd_auth_odbc:get_vh_registered_users_number(LServer).

-spec get_vh_registered_users_number(ejabberd:lserver(), list()) ->
    non_neg_integer().
get_vh_registered_users_number(LServer, Opts) ->
    ejabberd_auth_odbc:get_vh_registered_users_number(LServer, Opts).

-spec get_password(ejabberd:luser(), ejabberd:lserver()) ->
    scram:scram_tuple() | binary() | false.
get_password(LUser, LServer) ->
    ejabberd_auth_odbc:get_password(LUser, LServer).

-spec get_password_s(ejabberd:luser(), ejabberd:lserver()) -> binary().
get_password_s(LUser, LServer) ->
    ejabberd_auth_odbc:get_password_s(LUser, LServer).

-spec does_user_exist(ejabberd:luser(), ejabberd:lserver()) -> boolean().
does_user_exist(LUser, LServer) ->
    ejabberd_auth_odbc:does_user_exist(LUser, LServer).

-spec remove_user(ejabberd:luser(), ejabberd:lserver()) -> ok.
remove_user(LUser, LServer) ->
    ejabberd_auth_odbc:remove_user(LUser, LServer).

-spec remove_user(ejabberd:luser(), ejabberd:lserver(), binary()) ->
    no_return().
remove_user(LUser, LServer, Password) ->
    ejabberd_auth_odbc:remove_user(LUser, LServer, Password).

%%%------------------------------------------------------------------
%%% SCRAM
%%%------------------------------------------------------------------

-spec prepare_scrammed_password(pos_integer(), binary()) ->
    {binary(), binary()}.
prepare_scrammed_password(Iterations, Password) when is_integer(Iterations) ->
    PassDetailsEscaped = fun_chain:first(
        Password,
        scram:password_to_scram(Iterations),
        scram:serialize(),
        mongoose_rdbms:escape()
     ),
    {<<>>, PassDetailsEscaped}.

-spec prepare_password(ejabberd:server(), binary()) -> {binary(), binary()}.
prepare_password(Server, Password) ->
    case scram:enabled(Server) of
        true ->
            prepare_scrammed_password(scram:iterations(Server), Password);
        _ ->
            {mongoose_rdbms:escape(Password), <<>>}
    end.
