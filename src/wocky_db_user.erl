%%% @copyright 2015+ Hippware, Inc.
%%% @doc Wocky database interface for "users"
%%%
%%% == Terminology ==
%%% <dl>
%%% <dt>JID</dt>
%%% <dd>
%%%   Short for "Jabber IDentifier" and defined in RFC 6122, the JID is the
%%%   canonical identifier for a specific user within the Jabber network.
%%%
%%%   Briefly, the JID is composed of three parts: the "localpart" that
%%%   identifies the user, the "domainpart" that identifies the Jabber network
%%%   and the "resourcepart" that identifies the current connection to the
%%%   network.
%%%
%%% </dd>
%%% <dt>User/LUser</dt>
%%% <dd>
%%%   The "localpart" of the JID.
%%%
%%%   For Wocky it is a timeuuid generated when the user is created and
%%%   formatted as a canonical UUID string. This is the canonical user ID within
%%%   Wocky but is not meant for display to the end user.
%%%
%%%   The "L" prefix indicates that the string has been normalized according to
%%%   RFC 6122. Variables without the "L" prefix are assumed to not be
%%%   normalized and must be processed before use.
%%%
%%% </dd>
%%% <dt>Server/LServer</dt>
%%% <dd>
%%%   The "domainpart" of the JID.
%%%
%%%   For Wocky this is the virtual host that the user connects to. This is not
%%%   the physical machine that is handling the user's connection, but a domain
%%%   representing a single cluster of application instances.
%%%
%%%   The "L" prefix indicates that the string has been normalized according to
%%%   RFC 6122. Variables without the "L" prefix are assumed to not be
%%%   normalized and must be processed before use.
%%%
%%% </dd>
%%% <dt>Handle</dt>
%%% <dd>
%%%   This is the name that is displayed to the user and is chosen by the user
%%%   when they create their account.
%%%
%%%   It must be globally unique.
%%%
%%% </dd>
%%% </dl>
%%%
%%% @reference See <a href="http://xmpp.org/rfcs/rfc6122.html">RFC 6122</a>
%%% for more information on JIDs.
%%%
%%% @reference See <a href="https://tools.ietf.org/html/rfc4122">RFC 4122</a>
%%% for the definition of UUIDs.
%%%
%%% @reference
%%% <a href="https://en.wikipedia.org/wiki/Universally_unique_identifier">
%%% This Wikipedia article</a> provides a good overview of UUIDs.
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

%% @doc Generates a timeuuid in canonical text format for use as a user id.
-spec create_id() -> ejabberd:luser().
create_id() ->
    ossp_uuid:make(v1, text).


%% @equiv create_user(create_id(), LServer, Handle, Password)
%%
%% @see create_id/0
%% @see create_user/4
%%
-spec create_user(ejabberd:lserver(), handle(), password())
                 -> {ok, ejabberd:luser()} | {error, exists}.
create_user(LServer, Handle, Password) ->
    LUser = create_id(),
    case create_user(LUser, LServer, Handle, Password) of
        ok -> {ok, LUser};
        Error -> Error
    end.


%% @doc Creates a new user record in the database.
%%
%% `LUser': the "localpart" of the user's JID. Must be a unique timeuuid
%% generated for this user and in canonical string format.
%%
%% `LServer': the "domainpart" of the user's JID. Used to determine which
%% keyspace to store the user's data in.
%%
%% `Handle': the user's preferred display name. Must be globally unique.
%%
%% `Password': the processed password for the user. No encryption or obfuscation
%% is performed on the password at this level; it is stored as-is in the
%% database.
%%
-spec create_user(ejabberd:luser(), ejabberd:lserver(), handle(), password())
                 -> ok | {error, exists}.
create_user(LUser, LServer, Handle, Password) ->
    %% TODO: this really needs to be done in a batch, but we don't currently
    %% have a clean way to run queries in different keyspaces in the same batch.
    case create_handle_lookup(LUser, LServer, Handle) of
        true ->
            {ok, _} = create_user_record(LUser, LServer, Handle, Password),
            ok;

        false ->
            {error, exists}
    end.

%% @private
create_handle_lookup(LUser, LServer, Handle) ->
    Query = "INSERT INTO handle_to_user (user, server, handle)" ++
            " VALUES (?, ?, ?) IF NOT EXISTS",
    Values = #{user => LUser, server => LServer, handle => Handle},
    {ok, R} = wocky_db:query(shared, Query, Values, quorum),
    wocky_db:single_result(R).

%% @private
create_user_record(LUser, LServer, Handle, Password) ->
    Query = "INSERT INTO user (user, server, handle, password)" ++
            " VALUES (?, ?, ?, ?)",
    Values = #{user => LUser, server => LServer,
               handle => Handle, password => Password},
    wocky_db:query(LServer, Query, Values, quorum).


%% @doc Returns `true' if the user exists in the database.
%%
%% `LUser': the "localpart" of the user's JID.
%%
%% `LServer': the "domainpart" of the user's JID.
%%
-spec does_user_exist(ejabberd:luser(), ejabberd:lserver()) -> boolean().
does_user_exist(LUser, LServer) ->
    case get_handle(LUser, LServer) of
        {error, not_found} -> false;
        _Handle -> true
    end.


%% @doc Returns the user's handle.
%%
%% `LUser': the "localpart" of the user's JID.
%%
%% `LServer': the "domainpart" of the user's JID.
%%
-spec get_handle(ejabberd:luser(), ejabberd:lserver())
              -> handle() | {error, not_found}.
get_handle(LUser, LServer) ->
    Query = "SELECT handle FROM user WHERE user = ?",
    {ok, R} = wocky_db:query(LServer, Query, #{user => LUser}, quorum),
    case wocky_db:single_result(R) of
        undefined -> {error, not_found};
        Handle -> Handle
    end.


%% @doc Returns the user's password.
%%
%% `LUser': the "localpart" of the user's JID.
%%
%% `LServer': the "domainpart" of the user's JID.
%%
-spec get_password(ejabberd:luser(), ejabberd:lserver())
                  -> password() | {error, not_found}.
get_password(LUser, LServer) ->
    Query = "SELECT password FROM user WHERE user = ?",
    {ok, R} = wocky_db:query(LServer, Query, #{user => LUser}, quorum),
    case wocky_db:single_result(R) of
        undefined -> {error, not_found};
        Password -> Password
    end.


%% @doc Updates the user's password.
%%
%% `LUser': the "localpart" of the user's JID.
%%
%% `LServer': the "domainpart" of the user's JID.
%%
%% `Password': the processed password for the user. No encryption or obfuscation
%% is performed on the password at this level; it is stored as-is in the
%% database.
%%
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


%% @doc Removes the user from the database.
%%
%% `LUser': the "localpart" of the user's JID.
%%
%% `LServer': the "domainpart" of the user's JID.
%%
-spec remove_user(ejabberd:luser(), ejabberd:lserver()) -> ok.
remove_user(LUser, LServer) ->
    case get_handle(LUser, LServer) of
        {error, not_found} ->
            ok;

        Handle ->
            %% TODO: this really needs to be done in a batch, but we don't
            %% currently have a clean way to run queries in different keyspaces
            %% in the same batch.
            {ok, _} = remove_handle_lookup(Handle),
            {ok, _} = remove_user_record(LUser, LServer),
            ok
    end.

%% @private
remove_handle_lookup(Handle) ->
    Query = "DELETE FROM handle_to_user WHERE handle = ?",
    wocky_db:query(shared, Query, #{handle => Handle}, quorum).

%% @private
remove_user_record(LUser, LServer) ->
    Query = "DELETE FROM user WHERE user = ?",
    wocky_db:query(LServer, Query, #{user => LUser}, quorum).
