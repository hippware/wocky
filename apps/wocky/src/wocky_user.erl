%%% @copyright 2015+ Hippware, Inc.
%%% @doc Wocky user model

-module(wocky_user).

%% API
-export([get_users/1,
         create_user/3,
         does_user_exist/2,
         get_password/2,
         set_password/3,
         remove_user/2]).


%%%===================================================================
%%% API
%%%===================================================================

-spec get_users(Domain :: binary()) -> [binary()].
get_users(Domain) ->
    Query = <<"SELECT username FROM username_to_user WHERE domain = ?">>,
    {ok, Return} = cassandra:pquery(shared, Query, [Domain], quorum),
    Result = cassandra:rows(Return),
    lists:flatten(Result).


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
    Query = <<"SELECT id FROM username_to_user WHERE domain = ? AND username = ?">>,
    {ok, Return} = cassandra:pquery(shared, Query, [Domain, UserName], quorum),
    Result = cassandra:rows(Return),
    length(Result) > 0.


-spec get_password(Domain :: binary(),
                   UserName :: binary()
                  ) -> binary() | {error, not_found}.
get_password(Domain, UserName) ->
    Query = <<"SELECT password FROM user WHERE domain = ? AND username = ?">>,
    case cassandra:pquery(Domain, Query, [UserName], quorum) of
        {ok, Return} ->
            cassandra:single_result(Return);

        {error, _} = E -> E
    end.


-spec set_password(Domain :: binary(),
                   UserName :: binary(),
                   Password :: binary()
                  ) -> ok.
set_password(Domain, UserName, Password) ->
    Query = <<"UPDATE user SET password = ? WHERE username = ?">>,
    {ok, _} = cassandra:pquery(Domain, Query, [Password, UserName], quorum),
    ok.


-spec remove_user(Domain :: binary(), UserName :: binary()) -> ok.
remove_user(Domain, UserName) ->
    case remove_username_lookup(Domain, UserName) of
        true ->
            {ok, _} = remove_user_record(Domain, UserName),
            ok;

        false ->
            ok
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

create_user_id() ->
    {UUID, _} = uuid:get_v1(uuid:new(self())),
    UUID.

create_username_lookup(Id, Domain, UserName) ->
    Query = <<"INSERT INTO username_to_user (id, domain, username) VALUES (?, ?, ?) IF NOT EXISTS">>,
    {ok, Return} = cassandra:pquery(shared, Query, [Id, Domain, UserName], quorum),
    %% Note: Result is <<1>> for success, <<0>> if error.
    %% There is no documentation on the return type so it's possible,
    %%   in the future, this may not be a binary.
    cassandra:single_result(Return) /= <<0>>.

create_user_record(Id, Domain, UserName, Password) ->
    Query = <<"INSERT INTO user (id, domain, username, password) VALUES (?, ?, ?, ?)">>,
    cassandra:pquery(Domain, Query, [Id, Domain, UserName, Password], quorum).

remove_username_lookup(Domain, UserName) ->
    Query = <<"DELETE FROM username_to_user where domain = ? AND username = ?">>,
    {ok, Return} = cassandra:pquery(shared, Query, [Domain, UserName], quorum),
    %% Note: Result is <<1>> for success, <<0>> if error.
    %% There is no documentation on the return type so it's possible,
    %%   in the future, this may not be a binary.
    cassandra:single_result(Return) /= <<0>>.

remove_user_record(Domain, UserName) ->
    Query = <<"DELETE FROM user WHERE domain = ? AND username = ?">>,
    cassandra:pquery(Domain, Query, [Domain, UserName], quorum).
