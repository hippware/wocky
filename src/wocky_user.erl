%%% @copyright 2015+ Hippware, Inc.
%%% @doc Wocky user model

-module(wocky_user).

%% API
-export([create_user/3,
         does_user_exist/2,
         get_password/2,
         set_password/3,
         remove_user/2]).


%%%===================================================================
%%% API
%%%===================================================================

-spec create_user(Domain :: binary(),
                  UserName :: binary(),
                  Password :: binary()
                 ) -> ok | {error, exists}.
create_user(Domain, UserName, Password) ->
    Id = create_user_id(),
    case create_username_lookup(Id, Domain, UserName) of
        true ->
            {ok, _} = create_user_record(Id, Domain, UserName, Password),
            ok;

        false ->
            {error, exists}
    end.


-spec does_user_exist(Domin :: binary(), UserName :: binary()) -> boolean().
does_user_exist(Domain, UserName) ->
    case user_id_from_username(Domain, UserName) of
        undefined -> false;
        _         -> true
    end.


-spec get_password(Domain :: binary(),
                   UserName :: binary()
                  ) -> binary() | {error, not_found}.
get_password(Domain, UserName) ->
    case user_id_from_username(Domain, UserName) of
        undefined -> {error, not_found};
        Id ->
            Query = <<"SELECT password FROM user WHERE id = ?">>,
            {ok, Return} = cassandra:pquery(Domain, Query, [Id], quorum),
            cassandra:single_result(Return)
    end.


-spec set_password(Domain :: binary(),
                   UserName :: binary(),
                   Password :: binary()
                  ) -> ok | {error, not_found}.
set_password(Domain, UserName, Password) ->
    case user_id_from_username(Domain, UserName) of
        undefined -> {error, not_found};
        Id ->
            Query = <<"UPDATE user SET password = ? WHERE id = ?">>,
            {ok, _} = cassandra:pquery(Domain, Query, [Password, Id], quorum),
            ok
    end.


-spec remove_user(Domain :: binary(), UserName :: binary()) -> ok.
remove_user(Domain, UserName) ->
    case user_id_from_username(Domain, UserName) of
        undefined -> ok;
        Id ->
            {ok, _} = remove_username_lookup(Domain, UserName),
            {ok, _} = remove_user_record(Id, Domain),
            ok
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

create_user_id() ->
    ossp_uuid:make(v1, binary).

user_id_from_username(Domain, UserName) ->
    Query = <<"SELECT id FROM username_to_user WHERE domain = ? AND username = ?">>,
    {ok, Return} = cassandra:pquery(shared, Query, [Domain, UserName], quorum),
    cassandra:single_result(Return).

create_username_lookup(Id, Domain, UserName) ->
    Query = <<"INSERT INTO username_to_user (id, domain, username) VALUES (?, ?, ?) IF NOT EXISTS">>,
    {ok, Return} = cassandra:pquery(shared, Query, [Id, Domain, UserName], quorum),
    cassandra:boolean_result(Return).

create_user_record(Id, Domain, UserName, Password) ->
    Query = <<"INSERT INTO user (id, domain, username, password) VALUES (?, ?, ?, ?)">>,
    cassandra:pquery(Domain, Query, [Id, Domain, UserName, Password], quorum).

remove_username_lookup(Domain, UserName) ->
    Query = <<"DELETE FROM username_to_user WHERE domain = ? AND username = ?">>,
    cassandra:pquery(shared, Query, [Domain, UserName], quorum).

remove_user_record(Id, Domain) ->
    Query = <<"DELETE FROM user WHERE id = ?">>,
    cassandra:pquery(Domain, Query, [Id], quorum).
