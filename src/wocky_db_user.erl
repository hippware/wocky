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

-include("wocky.hrl").

-type handle()       :: binary().
-type phone_number() :: binary().
-type password()     :: binary().
-type token()        :: binary().
-export_type([handle/0, phone_number/0, password/0, token/0]).

%% API
-export([register_user/3,
         register_user/2,
         update_user/3,
         remove_user/2,
         does_user_exist/2,
         find_user/2,
         find_user_by/2,
         get_handle/2,
         get_phone_number/2,
         get_password/2,
         set_password/3,
         set_location/6,
         assign_token/3,
         release_token/3,
         check_token/3,
         add_roster_viewer/3,
         remove_roster_viewer/3,
         get_roster_viewers/2]).

-ifdef(TEST).
-export([generate_token/0, get_tokens/2]).
-endif.

-compile({parse_transform, do}).


%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates or updates a new user record in the database. This is meant
%% to be called by internal code when using the in-band registration and
%% admin user management modules. Therefore, we assume that the username is
%% correct and update the user record if it already exists.
%%
%% `LUser': the "localpart" of the user's JID. Must be a unique timeuuid
%% generated for this user and in canonical string format.
%%
%% `LServer': the "domainpart" of the user's JID. Used to determine which
%% keyspace to store the user's data in.
%%
%% `Password': the processed password for the user. No encryption or obfuscation
%% is performed on the password at this level; it is stored as-is in the
%% database.
%%
-spec register_user(ejabberd:luser(), ejabberd:lserver(), password()) -> ok.
register_user(LUser, LServer, Password) ->
    wocky_db:insert(shared, user,
                    #{user => LUser,
                      server => LServer,
                      password => Password}).


%% @doc Creates or updates a user based on the external authentication ID and
%% phone number.
%%
%% `ExternalId': the external authentication id.
%%
%% `PhoneNumber': the phone number for the user.
%%
-spec register_user(binary(), binary())
        -> {ok, {binary(), binary(), boolean()}}.
register_user(ExternalId, PhoneNumber) ->
    {User, Server, _} = Ret = assign_server_and_id(ExternalId, PhoneNumber),
    Queries = [update_user_record_query(User, Server, ExternalId),
               update_phone_number_query(PhoneNumber, User, Server)],
    {ok, void} = wocky_db:batch_query(shared, Queries, quorum),
    {ok, Ret}.

%% @private
assign_server_and_id(ExternalId, PhoneNumber) ->
    case lookup_userid(external_id_to_user, external_id, ExternalId) of
        not_found ->
            {wocky_db:create_id(),
             assign_server(PhoneNumber),
             true};

        {User, Server} ->
            {User, Server, false}
    end.

%% @private
assign_server(_PhoneNumber) ->
    %% TODO: This is where the code to assign a user to a particular server
    %% will go:
    wocky_app:server().

%% @private
update_user_record_query(User, Server, ExternalId) ->
    {"INSERT INTO user (user, server, external_id) VALUES (?, ?, ?)",
     #{user => User, server => Server, external_id => ExternalId}}.

%% @private
update_phone_number_query(PhoneNumber, User, Server) ->
    {"INSERT INTO phone_number_to_user (phone_number, user, server)"
     " VALUES (?, ?, ?)",
     #{phone_number => PhoneNumber, user => User, server => Server}}.


%% @doc Update the data on an existing user.
%%
%% `LUser': the "localpart" of the user's JID.
%%
%% `LServer': the "domainpart" of the user's JID.
%%
%% `Fields': is a map containing fields to update. Valid keys are `handle',
%%           `avatar', `first_name', `last_name' and `email'. All other keys
%%           are ignored.
%%
-spec update_user(binary(), binary(), map()) -> ok | {error, atom()}.
update_user(User, Server, Fields) ->
    UpdateFields = maps:with(valid_user_fields(), Fields),
    do([error_m ||
        UserData <- maybe_lookup_user(should_lookup_user(Fields), User, Server),
        prepare_avatar(User, Server, Fields),
        update_handle_lookup(User, Server, UserData, Fields),
        delete_existing_avatar(UserData),
        do_update_user(User, Server, UpdateFields, maps:size(UpdateFields))
       ]).

%% @private
do_update_user(_, _, _, 0) ->
    ok;
do_update_user(User, Server, UpdateFields, _) ->
    ok = wocky_db:update(shared, user, UpdateFields,
                         #{user => User, server => Server}),
    wocky_db_user_idx:user_updated(User, UpdateFields).

%% @private
valid_user_fields() ->
    [handle, avatar, first_name, last_name, email].

%% @private
should_lookup_user(Fields) ->
    maps:is_key(avatar, Fields) orelse maps:is_key(handle, Fields).

%% @private
maybe_lookup_user(false, _User, _Server) -> {ok, #{}};
maybe_lookup_user(true, User, Server) ->
    case find_user(User, Server) of
        not_found -> {ok, #{}};
        UserData -> {ok, UserData}
    end.

%% @private
prepare_avatar(UserID, LServer, #{avatar := NewAvatar}) ->
    do([error_m ||
        {FileServer, FileID} <- tros:parse_url(NewAvatar),
        check_file_location(LServer, FileServer),
        File <- francus:open_read(LServer, FileID),
        check_avatar_owner(UserID, File),
        check_avatar_purpose(File),
        francus:keep(LServer, francus:id(File))
       ]);
prepare_avatar(_, _, _) -> ok.

%% @private
check_file_location(Server, Server) -> ok;
check_file_location(_, _) -> {error, not_local_file}.

%% @private
check_avatar_owner(UserID, File) ->
    case francus:owner(File) of
        UserID -> ok;
        _ -> {error, not_file_owner}
    end.

%% @private
check_avatar_purpose(File) ->
    case francus:purpose(File) of
        <<"avatar">> ->
            ok;
        _ ->
            {error, not_avatar_file}
    end.

%% @private
update_handle_lookup(User, Server, #{handle := OldHandle},
                     #{handle := NewHandle}) when OldHandle =/= NewHandle ->
    %% Unfortunately we cannot run these queries in a batch. The LWT means
    %% that the batch will only work if the records are all in the same
    %% partition, and since the handle is the partition key, this will never
    %% be the case.
    Values = #{user => User, server => Server, handle => NewHandle},
    case wocky_db:insert_new(shared, handle_to_user, Values) of
        true -> delete_old_handle(OldHandle);
        false -> {error, duplicate_handle}
    end;
update_handle_lookup(_, _, _, _) -> ok.

%% @private
delete_old_handle(null) -> ok;
delete_old_handle(OldHandle) ->
    wocky_db:delete(shared, handle_to_user, all, #{handle => OldHandle}).

%% @private
delete_existing_avatar(#{avatar := OldAvatar}) ->
    case tros:parse_url(OldAvatar) of
        {ok, {Server, FileID}} -> francus:delete(Server, FileID);
        {error, _} -> ok
    end;
delete_existing_avatar(_) -> ok.


%% @doc Removes the user from the database.
%%
%% `LUser': the "localpart" of the user's JID.
%%
%% `LServer': the "domainpart" of the user's JID.
%%
-spec remove_user(ejabberd:luser(), ejabberd:lserver()) -> ok.
remove_user(LUser, LServer) ->
    ok = remove_shared_user_data(LUser, LServer),
    ok = remove_local_user_data(LUser, LServer),
    ok = wocky_db_user_idx:user_removed(LUser),
    ok.

%% @private
remove_shared_user_data(LUser, LServer) ->
    Handle = get_handle(LUser, LServer),
    PhoneNumber = get_phone_number(LUser, LServer),
    Queries = [remove_handle_lookup_query(Handle),
               remove_phone_lookup_query(PhoneNumber),
               remove_user_record_query(LUser, LServer)],
    {ok, void} = wocky_db:batch_query(shared, lists:flatten(Queries), quorum),
    ok.

%% @private
remove_local_user_data(LUser, LServer) ->
    Queries = [remove_tokens_query(LUser, LServer),
               remove_locations_query(LUser, LServer)],
    {ok, void} = wocky_db:batch_query(LServer, Queries, quorum),
    ok.

%% @private
remove_handle_lookup_query(not_found) -> [];
remove_handle_lookup_query(Handle) ->
    {"DELETE FROM handle_to_user WHERE handle = ?",
     #{handle => Handle}}.

%% @private
remove_phone_lookup_query(not_found) -> [];
remove_phone_lookup_query(PhoneNumber) ->
    {"DELETE FROM phone_number_to_user WHERE phone_number = ?",
     #{phone_number => PhoneNumber}}.

%% @private
remove_user_record_query(LUser, LServer) ->
    {"DELETE FROM user WHERE user = ? AND server = ?",
     #{user => LUser, server => LServer}}.

%% @private
remove_tokens_query(LUser, LServer) ->
    {"DELETE FROM auth_token WHERE user = ? AND server = ?",
     #{user => LUser, server => LServer}}.

%% @private
remove_locations_query(LUser, LServer) ->
    {"DELETE FROM location WHERE user = ? AND server = ?",
     #{user => LUser, server => LServer}}.


%% @doc Returns `true' if the user exists in the database.
%%
%% `LUser': the "localpart" of the user's JID.
%%
%% `LServer': the "domainpart" of the user's JID.
%%
-spec does_user_exist(ejabberd:luser(), ejabberd:lserver()) -> boolean().
does_user_exist(<<>>, _LServer) ->
    false;
does_user_exist(LUser, LServer) ->
    case find_user(LUser, LServer) of
        not_found -> false;
        _UserData -> true
    end.


%% @doc Returns a map of all fields for a given user or `not_found' if no such
%% user exists.
%%
%% `LUser': the "localpart" of the user's JID.
%%
%% `LServer': the "domainpart" of the user's JID.
%%
-spec find_user(ejabberd:lserver(), ejabberd:lserver()) -> map() | not_found.
find_user(LUser, LServer) ->
    Conditions = #{user => LUser, server => LServer},
    wocky_db:select_row(shared, user, all, Conditions).


%% @doc Returns a map of all fields for a given user or `not_found' if no such
%% user exists.
%%
%% `Key': the key to use to lookup the user. Acceptable values include
%%        `handle', `phone_number' and `external_id'.
%%
%% `Value': the value that corresponds to `Key'.
%%
-spec find_user_by(Key :: atom(), Value :: binary()) -> map() | not_found.
find_user_by(handle, Handle) ->
    find_user_by_lookup(handle_to_user, handle, Handle);
find_user_by(phone_number, PhoneNumber) ->
    find_user_by_lookup(phone_number_to_user, phone_number, PhoneNumber);
find_user_by(external_id, ExternalId) ->
    find_user_by_lookup(external_id_to_user, external_id, ExternalId).

%% @private
find_user_by_lookup(Table, Col, Value) ->
    case lookup_userid(Table, Col, Value) of
        {User, Server} -> find_user(User, Server);
        not_found -> not_found
    end.

%% @private
lookup_userid(Table, Col, Value) ->
    case wocky_db:select_row(shared, Table, [user, server], #{Col => Value}) of
        not_found -> not_found;
        #{user := User, server := Server} -> {User, Server}
    end.


%% @doc Returns the user's handle.
%%
%% `LUser': the "localpart" of the user's JID.
%%
%% `LServer': the "domainpart" of the user's JID.
%%
-spec get_handle(ejabberd:luser(), ejabberd:lserver())
                -> handle() | not_found.
get_handle(LUser, LServer) ->
    Conditions = #{user => LUser, server => LServer},
    case wocky_db:select_one(shared, user, handle, Conditions) of
        not_found -> not_found;
        null -> not_found;
        Value -> Value
    end.


%% @doc Returns the user's phone number.
%%
%% `LUser': the "localpart" of the user's JID.
%%
%% `LServer': the "domainpart" of the user's JID.
%%
-spec get_phone_number(ejabberd:luser(), ejabberd:lserver())
                      -> binary() | not_found.
get_phone_number(LUser, _LServer) ->
    wocky_db:select_one(shared, user_to_phone_number, phone_number,
                        #{user => LUser}).


%% @doc Returns the user's password.
%%
%% `LUser': the "localpart" of the user's JID.
%%
%% `LServer': the "domainpart" of the user's JID.
%%
-spec get_password(ejabberd:luser(), ejabberd:lserver())
                  -> password() | not_found.
get_password(LUser, LServer) ->
    Conditions = #{user => LUser, server => LServer},
    wocky_db:select_one(shared, user, password, Conditions).


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
                  -> ok | not_found.
set_password(LUser, LServer, Password) ->
    case does_user_exist(LUser, LServer) of
        false -> not_found;
        true -> wocky_db:update(shared, user,
                                #{password => Password},
                                #{user => LUser, server => LServer})
    end.


%% @doc Updates the user's location.
%%
%% `LUser': the "localpart" of the user's JID.
%%
%% `LServer': the "domainpart" of the user's JID.
%%
%% `LResource': the "resourcepart" of the user's JID.
%%
%% `Lat': the latitude of the user's location in degrees North
%%
%% `Lon': the longditude of the user's location in degrees East
%%
%% `Accuracy': the accuracy of the user's location in meters
%%
-spec set_location(ejabberd:luser(), ejabberd:lserver(), ejabberd:lresource(),
                   number(), number(), number()) -> ok.
set_location(LUser, LServer, LResource, Lat, Lon, Accuracy) ->
    wocky_db:insert(LServer, location, #{user =>     LUser,
                                         server =>   LServer,
                                         resource => LResource,
                                         time =>     now,
                                         lat =>      Lat,
                                         lon =>      Lon,
                                         accuracy => Accuracy}).


%% @doc Generates a token.
-spec generate_token() -> token().
generate_token() ->
    RandomBytes = crypto:strong_rand_bytes(?TOKEN_BYTES),
    String = base64:encode_to_string(RandomBytes),
    iolist_to_binary([?TOKEN_MARKER, String]).


%% @doc Generates a token and assigns it to the specified user and resource.
%%
%% `LUser': the "localpart" of the user's JID.
%%
%% `LServer': the "domainpart" of the user's JID.
%%
%% `LResource': the "resourcepart" of the user's JID.
%%
-spec assign_token(ejabberd:luser(), ejabberd:lserver(), ejabberd:lresource())
                  -> {ok, token(), pos_integer()} | not_found.
assign_token(LUser, LServer, LResource) ->
    case does_user_exist(LUser, LServer) of
        true ->
            Token = generate_token(),
            CreatedAt = wocky_db:now_to_timestamp(os:timestamp()),
            ExpiresAt = CreatedAt + ?TOKEN_EXPIRE,
            ok = wocky_db:insert(LServer, auth_token,
                                 #{user => LUser,
                                   server => LServer,
                                   resource => LResource,
                                   auth_token => Token,
                                   created_at => CreatedAt,
                                   expires_at => ExpiresAt,
                                   '[ttl]' => ?TOKEN_EXPIRE}),
            {ok, Token, ExpiresAt};

        false ->
            not_found
    end.


%% @doc Releases any token currently assigned to the specified user and
%% resource.
%%
%% `LUser': the "localpart" of the user's JID.
%%
%% `LServer': the "domainpart" of the user's JID.
%%
%% `LResource': the "resourcepart" of the user's JID.
%%
-spec release_token(ejabberd:luser(), ejabberd:lserver(), ejabberd:lresource())
                   -> ok.
release_token(LUser, LServer, LResource) ->
    wocky_db:delete(LServer, auth_token, all, #{user => LUser,
                                                server => LServer,
                                                resource => LResource}).


%% @doc Returns all tokens currently assigned to resources belonging to the
%% specified user.
%%
%% `LUser': the "localpart" of the user's JID.
%%
%% `LServer': the "domainpart" of the user's JID.
%%
-spec get_tokens(ejabberd:luser(), ejabberd:lserver()) -> [token()].
get_tokens(LUser, LServer) ->
    Rows = wocky_db:select(LServer, auth_token, [auth_token],
                           #{user => LUser, server => LServer}),
    [Token || #{auth_token := Token} <- Rows].


%% @doc Returns `true' if a token is valid for the supplied
%% user or `false' otherwise.
%%
%% `LUser': the "localpart" of the user's JID.
%%
%% `LServer': the "domainpart" of the user's JID.
%%
%% `Token': the token to test
%%
-spec check_token(ejabberd:lserver(), ejabberd:luser(), token()) -> boolean().
check_token(LUser, LServer, Token) ->
    lists:member(Token, get_tokens(LUser, LServer)).

%% @doc Adds a roster viewer to a user.
%%
%% `LUser': the "localpart" of the user's JID.
%%
%% `LServer': the "domainpart" of the user's JID.
%%
%% `ViewerJID': The JID of the entity able to view the user's roster
%%
-spec add_roster_viewer(ejabberd:luser(), ejabberd:lserver(),
                        ejabberd:jid()) ->
    ok | not_found.
add_roster_viewer(LUser, LServer, ViewerJID) ->
    update_roster_viewer("+", LUser, LServer, ViewerJID).

%% @doc Removes a roster viewer from a user.
%%
%% `LUser': the "localpart" of the user's JID.
%%
%% `LServer': the "domainpart" of the user's JID.
%%
%% `ViewerJID': The JID of the entity to remove from permitted viewers
%%
-spec remove_roster_viewer(ejabberd:luser(), ejabberd:lserver(),
                           ejabberd:jid()) ->
    ok | not_found.
remove_roster_viewer(LUser, LServer, ViewerJID) ->
    update_roster_viewer("-", LUser, LServer, ViewerJID).

%% @private
update_roster_viewer(Op, LUser, LServer, ViewerJID) ->
    Q = ["UPDATE user SET roster_viewers = roster_viewers ", Op,
         " ? WHERE user = ? AND server = ? IF EXISTS"],
    V = #{user => LUser, server => LServer,
          roster_viewers => [jid:to_binary(ViewerJID)]},
    parse_viewer_result(wocky_db:query(shared, Q, V, quorum)).

%% @private
parse_viewer_result({ok, Result}) ->
    case wocky_db:single_result(Result) of
        true -> ok;
        false -> not_found
    end.

%% @doc Gets the list of entities allowed to view a user's roster.
%% Returns a list of viewers, or `not_found' if the user does not exist.
%%
%% `LUser': the "localpart" of the user's JID.
%%
%% `LServer': the "domainpart" of the user's JID.
%%
-spec get_roster_viewers(ejabberd:luser(), ejabberd:lserver()) ->
    [binary()] | not_found.
get_roster_viewers(LUser, LServer) ->
    V = #{user => LUser, server => LServer},
    case wocky_db:select_one(shared, user, roster_viewers, V) of
        null -> [];
        not_found -> not_found;
        List -> List
    end.
