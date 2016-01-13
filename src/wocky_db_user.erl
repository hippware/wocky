%%% @copyright 2015+ Hippware, Inc.
%%% @doc Wocky user model

-module(wocky_db_user).

-type id()       :: binary().
-type name()     :: binary().
-type password() :: binary().
-export_type([id/0, name/0, password/0]).

%% API
-export([create_id/0,
         create_user/3,
         create_user/4,
         does_user_exist/2,
         get_name/2,
         get_password/2,
         set_password/3,
         remove_user/2]).


%%%===================================================================
%%% API
%%%===================================================================

-spec create_id() -> ejabberd:luser().
create_id() ->
    ossp_uuid:make(v1, text).


-spec create_user(ejabberd:lserver(), name(), password())
                 -> {ok, ejabberd:luser()} | {error, exists}.
create_user(LServer, Name, Password) ->
    LUser = create_id(),
    case create_user(LUser, LServer, Name, Password) of
        ok -> {ok, LUser};
        Error -> Error
    end.


-spec create_user(ejabberd:luser(), ejabberd:lserver(), name(), password())
                 -> ok | {error, exists}.
create_user(LUser, LServer, Name, Password) ->
    %% TODO this really needs to be done in a batch, but we don't currently
    %% have a clean way to run queries in different keyspaces in the same batch.
    case create_username_lookup(LUser, LServer, Name) of
        true ->
            {ok, _} = create_user_record(LUser, LServer, Name, Password),
            ok;

        false ->
            {error, exists}
    end.


-spec does_user_exist(ejabberd:luser(), ejabberd:lserver()) -> boolean().
does_user_exist(LUser, LServer) ->
    case get_name(LUser, LServer) of
        {error, not_found} -> false;
        _Name -> true
    end.


-spec get_name(ejabberd:luser(), ejabberd:lserver())
              -> name() | {error, not_found}.
get_name(LUser, LServer) ->
    Query = "SELECT username FROM user WHERE id = ?",
    {ok, R} = wocky_db:query(LServer, Query, #{id => LUser}, quorum),
    case wocky_db:single_result(R) of
        undefined -> {error, not_found};
        Name -> Name
    end.


-spec get_password(ejabberd:luser(), ejabberd:lserver())
                  -> password() | {error, not_found}.
get_password(LUser, LServer) ->
    Query = "SELECT password FROM user WHERE id = ?",
    {ok, R} = wocky_db:query(LServer, Query, #{id => LUser}, quorum),
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
            Query = "UPDATE user SET password = ? WHERE id = ?",
            Values = #{password => Password, id => LUser},
            {ok, _} = wocky_db:query(LServer, Query, Values, quorum),
            ok
    end.


-spec remove_user(ejabberd:luser(), ejabberd:lserver()) -> ok.
remove_user(LUser, LServer) ->
    case get_name(LUser, LServer) of
        {error, not_found} ->
            ok;

        Name ->
            %% TODO this really needs to be done in a batch, but we don't
            %% currently have a clean way to run queries in different keyspaces
            %% in the same batch.
            {ok, _} = remove_username_lookup(LServer, Name),
            {ok, _} = remove_user_record(LUser, LServer),
            ok
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

create_username_lookup(LUser, LServer, Name) ->
    Query = "INSERT INTO username_to_user (id, domain, username)" ++
            " VALUES (?, ?, ?) IF NOT EXISTS",
    Values = #{id => LUser, domain => LServer, username => Name},
    {ok, R} = wocky_db:query(shared, Query, Values, quorum),
    wocky_db:single_result(R).

create_user_record(LUser, LServer, Name, Password) ->
    Query = "INSERT INTO user (id, domain, username, password)" ++
            " VALUES (?, ?, ?, ?)",
    Values = #{id => LUser, domain => LServer,
               username => Name, password => Password},
    wocky_db:query(LServer, Query, Values, quorum).

remove_username_lookup(LServer, Name) ->
    Query = "DELETE FROM username_to_user WHERE domain = ? AND username = ?",
    Values = #{domain => LServer, username => Name},
    wocky_db:query(shared, Query, Values, quorum).

remove_user_record(LUser, LServer) ->
    Query = "DELETE FROM user WHERE id = ?",
    wocky_db:query(LServer, Query, #{id => LUser}, quorum).
