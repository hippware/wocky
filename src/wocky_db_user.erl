%%% @copyright 2015+ Hippware, Inc.
%%% @doc Wocky user model
%%%
%%% Terminology
%%% User/LUser:
%%%   The local part of the JID. For us, it is a timeuuid generated when the
%%%   user is created. This is not meant for display to the end user.
%%%
%%% Server/LServer:
%%%   The virtual host that the user connects to. This is not the physical
%%%   machine that is handling the user's connection, but a domain representing
%%%   a single cluster of application instances.
%%%
%%% Handle:
%%%   This is the name that is displayed to the user and is chosen by the user
%%%   when they create their account. It must be globally unique.
%%%
-module(wocky_db_user).

-type handle()   :: binary().
-type password() :: binary().
-export_type([handle/0, password/0]).

%% API
-export([create_id/0,
         create_user/3,
         create_user/4,
         does_user_exist/2,
         get_handle/2,
         get_password/2,
         set_password/3,
         remove_user/2]).


%%%===================================================================
%%% API
%%%===================================================================

-spec create_id() -> ejabberd:luser().
create_id() ->
    ossp_uuid:make(v1, text).


-spec create_user(ejabberd:lserver(), handle(), password())
                 -> {ok, ejabberd:luser()} | {error, exists}.
create_user(LServer, Handle, Password) ->
    LUser = create_id(),
    case create_user(LUser, LServer, Handle, Password) of
        ok -> {ok, LUser};
        Error -> Error
    end.


-spec create_user(ejabberd:luser(), ejabberd:lserver(), handle(), password())
                 -> ok | {error, exists}.
create_user(LUser, LServer, Handle, Password) ->
    %% TODO this really needs to be done in a batch, but we don't currently
    %% have a clean way to run queries in different keyspaces in the same batch.
    case create_handle_lookup(LUser, LServer, Handle) of
        true ->
            {ok, _} = create_user_record(LUser, LServer, Handle, Password),
            ok;

        false ->
            {error, exists}
    end.


-spec does_user_exist(ejabberd:luser(), ejabberd:lserver()) -> boolean().
does_user_exist(LUser, LServer) ->
    case get_handle(LUser, LServer) of
        {error, not_found} -> false;
        _Handle -> true
    end.


-spec get_handle(ejabberd:luser(), ejabberd:lserver())
              -> handle() | {error, not_found}.
get_handle(LUser, LServer) ->
    Query = "SELECT handle FROM user WHERE user = ?",
    {ok, R} = wocky_db:query(LServer, Query, #{user => LUser}, quorum),
    case wocky_db:single_result(R) of
        undefined -> {error, not_found};
        Name -> Name
    end.


-spec get_password(ejabberd:luser(), ejabberd:lserver())
                  -> password() | {error, not_found}.
get_password(LUser, LServer) ->
    Query = "SELECT password FROM user WHERE user = ?",
    {ok, R} = wocky_db:query(LServer, Query, #{user => LUser}, quorum),
    case wocky_db:single_result(R) of
        undefined -> {error, not_found};
        Password -> Password
    end.


-spec set_password(ejabberd:luser(), ejabberd:lserver(), password())
                  -> ok | {error, not_found}.
set_password(LUser, LServer, Password) ->
    case does_user_exist(LUser, LServer) of
        false -> {error, not_found};
        true ->
            Query = "UPDATE user SET password = ? WHERE user = ?",
            Values = #{password => Password, user => LUser},
            {ok, _} = wocky_db:query(LServer, Query, Values, quorum),
            ok
    end.


-spec remove_user(ejabberd:luser(), ejabberd:lserver()) -> ok.
remove_user(LUser, LServer) ->
    case get_handle(LUser, LServer) of
        {error, not_found} ->
            ok;

        Handle ->
            %% TODO this really needs to be done in a batch, but we don't
            %% currently have a clean way to run queries in different keyspaces
            %% in the same batch.
            {ok, _} = remove_handle_lookup(Handle),
            {ok, _} = remove_user_record(LUser, LServer),
            ok
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

create_handle_lookup(LUser, LServer, Handle) ->
    Query = "INSERT INTO handle_to_user (user, server, handle)" ++
            " VALUES (?, ?, ?) IF NOT EXISTS",
    Values = #{user => LUser, server => LServer, handle => Handle},
    {ok, R} = wocky_db:query(shared, Query, Values, quorum),
    wocky_db:single_result(R).

create_user_record(LUser, LServer, Handle, Password) ->
    Query = "INSERT INTO user (user, server, handle, password)" ++
            " VALUES (?, ?, ?, ?)",
    Values = #{user => LUser, server => LServer,
               handle => Handle, password => Password},
    wocky_db:query(LServer, Query, Values, quorum).

remove_handle_lookup(Handle) ->
    Query = "DELETE FROM handle_to_user WHERE handle = ?",
    wocky_db:query(shared, Query, #{handle => Handle}, quorum).

remove_user_record(LUser, LServer) ->
    Query = "DELETE FROM user WHERE user = ?",
    wocky_db:query(LServer, Query, #{user => LUser}, quorum).
