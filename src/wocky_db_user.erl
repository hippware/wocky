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
-type auth_name()    :: binary().
-type password()     :: binary().
-type token()        :: binary().
-export_type([handle/0, phone_number/0, password/0, token/0]).

%% API
-export([create_user/3,
         create_user/4,
         create_user/1,
         update_user/1,
         maybe_set_handle/3,
         set_phone_number/3,
         does_user_exist/2,
         get_handle/2,
         get_phone_number/2,
         get_password/2,
         set_password/3,
         remove_user/2,
         generate_token/0,
         assign_token/3,
         release_token/3,
         get_tokens/2,
         check_token/3,
         check_token/4,
         get_user_data/2,
         get_user_by_external_id/2,
         get_user_by_handle/1,
         get_user_by_phone_number/1]).

-compile({parse_transform, do}).


%%%===================================================================
%%% API
%%%===================================================================

%% @equiv create_user(wocky_db:create_id(), LServer, Handle, Password)
%%
%% @see wocky_db:create_id/0
%% @see create_user/4
%%
-spec create_user(ejabberd:lserver(), handle(), password())
                 -> {ok, ejabberd:luser()} | {error, exists}.
create_user(LServer, Handle, Password) ->
    LUser = wocky_db:create_id(),
    case create_user(LUser, LServer, Handle, Password) of
        ok -> {ok, LUser};
        Error -> Error
    end.


%% @doc Create a user based on the fields supplied in `Fields. This function
%% will <b>NOT</b> fill in `handle' nor `phone_number' fields since these
%% can clash with existing entries while the rest of the user data remains
%% valid. They must be added (and checked) separately using
%% {@link maybe_set_handle/3} and {@link set_phone_number/3}.
%%
%% `Fields' is a map containing fields to set. At a minimum, `server', must
%% be set.
-spec create_user(map()) -> ejabberd:luser() | {error, atom()}.
create_user(Fields) ->
    NewID = wocky_db:create_id(),
    WithUser = Fields#{user => NewID},
    CreationFields = maps:without([handle, phone_number], WithUser),
    do([error_m ||
        maybe_update_avatar(Fields),
        error_m:return(
          true = wocky_db:insert_new(shared, user, CreationFields)),
        NewID]).

%% @doc Update the data on an existing user. As with {@link create_user/1},
%% `handle' and `phone_number' will be ignored and must be set separately.
%%
%% `Fields' is a map containing fields to update. At a minimum, `server' and
%% `user' must be set.
-spec update_user(map()) -> ok | {error, atom()}.
update_user(Fields = #{user := User}) ->
    UpdateFields = maps:without([user, handle, phone_number], Fields),
    do([error_m ||
        maybe_update_avatar(Fields),
        wocky_db:update(shared, user, UpdateFields, #{user => User})
       ]).

%% @private
maybe_update_avatar(#{user := UserID, avatar := Avatar, server := LServer}) ->
    do([error_m ||
        {FileServer, FileID} <- tros:parse_url(Avatar),
        check_file_location(LServer, FileServer),
        File <- francus:open_read(LServer, FileID),
        check_avatar_owner(UserID, File),
        check_avatar_purpose(UserID, LServer, File),
        francus:keep(LServer, francus:id(File)),
        assign_avatar(UserID, LServer, Avatar)
       ]);

maybe_update_avatar(_) -> ok.

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
check_avatar_purpose(UserID, LServer, File) ->
    UserJID = jid:to_binary(jid:make(UserID, LServer, <<>>)),
    case francus:metadata(File) of
        #{<<"purpose">> := <<"avatar:", UserJID/binary>>} ->
            ok;
        _ ->
            {error, not_avatar_file}
    end.

%% @private
assign_avatar(UserID, LServer, Avatar) ->
    maybe_delete_existing_avatar(UserID, LServer),
    wocky_db:update(shared, user, #{avatar => Avatar}, #{user => UserID}).

%% @private
maybe_delete_existing_avatar(UserID, _LServer) ->
    case wocky_db:select_one(shared, user, avatar, #{user => UserID}) of
        URL = <<"tros:", _/binary>> ->
            {ok, {Server, FileID}} = tros:parse_url(URL),
            francus:delete(Server, FileID);
        _ ->
            ok
    end.


%% @doc Attempts to set a handle for a given user. The attempt will fail
%% and `false' returned if the requested handle is already held by another
%% user. On success, returns `true'.
%%
%% `LUser': the "localpart" of the user's JID.
%%
%% `LServer': the "domainpart" of the user's JID.
%%
%% `Handle': the user's preferred display name.
%%
-spec maybe_set_handle(ejabberd:luser(), ejabberd:lserver(), handle())
        -> boolean().
maybe_set_handle(LUser, LServer, Handle) ->
    maybe_create_handle(LUser, LServer, Handle)
    andalso
    update_lookup(LUser, LServer, handle_to_user, handle, Handle).

%% @private
maybe_create_handle(LUser, LServer, Handle) ->
    Values = #{user => LUser, server => LServer, handle => Handle},
    wocky_db:insert_new(shared, handle_to_user, Values).


%% @doc Sets the phone number for a given user. If a different user already
%% has the same phone number, it will be entirely removed from their record.
%%
%% `LUser': the "localpart" of the user's JID.
%%
%% `LServer': the "domainpart" of the user's JID.
%%
%% `PhoneNumber': the phone number to assign to the user.
%%
-spec set_phone_number(ejabberd:luser(), ejabberd:lserver(), phone_number())
        -> ok.
set_phone_number(LUser, LServer, PhoneNumber) ->
    create_phone_number_lookup(LUser, LServer, PhoneNumber),
    update_lookup(LUser, LServer, phone_number_to_user,
                  phone_number, PhoneNumber),
    ok.

%% @private
create_phone_number_lookup(LUser, LServer, PhoneNumber) ->
    maybe_remove_phone_number_from_other_user(LUser, LServer, PhoneNumber),
    Values = #{user => LUser, server => LServer},
    Conditions = #{phone_number => PhoneNumber},
    ok = wocky_db:update(shared, phone_number_to_user, Values, Conditions).

%% @private
maybe_remove_phone_number_from_other_user(LUser, LServer, PhoneNumber) ->
    case get_user_by_phone_number(PhoneNumber) of
        not_found -> ok;
        {LUser, LServer} -> ok;
        {OtherUser, _OtherServer} ->
            ok = wocky_db:update(shared, user, #{phone_number => null},
                                 #{user => OtherUser})
    end.

%% @private
update_lookup(LUser, _LServer, Table, Col, Key) ->
    case wocky_db:select_one(shared, user, Col, #{user => LUser}) of
        K when K =:= null; K =:= Key ->
            ok;
        OldKey ->
            ok = wocky_db:delete(shared, Table, all, #{Col => OldKey})
    end,
    ok = wocky_db:update(shared, user, #{Col => Key}, #{user => LUser}),
    true.


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
-spec create_user(ejabberd:luser(), ejabberd:lserver(),
                  handle(), password())
                 -> ok | {error, exists | invalid_id}.
create_user(LUser, LServer, Handle, Password) ->
    create_user(LUser, LServer, Handle, Password, wocky_db:is_valid_id(LUser)).

%% @private
create_user(LUser, LServer, Handle, Password, true) ->
    %% TODO: this really needs to be done in a batch, but we don't currently
    %% have a clean way to run queries in different keyspaces in the same batch.
    Res = maybe_create_handle(LUser, LServer, Handle),
    case Res of
        true ->
            create_user_record(LUser, LServer, Handle, Password);

        false ->
            {error, exists}
    end;
create_user(_, _, _, _, false) ->
    {error, invalid_id}.

%% @private
create_user_record(LUser, LServer, Handle, Password) ->
    Values = #{user => LUser, server => LServer,
               handle => Handle, password => Password},
    wocky_db:insert(shared, user, Values).


%% @doc Returns `true' if the user exists in the database.
%%
%% `LUser': the "localpart" of the user's JID.
%%
%% `LServer': the "domainpart" of the user's JID.
%%
-spec does_user_exist(ejabberd:luser(), ejabberd:lserver()) -> boolean().
does_user_exist(<<>>, _LServer) ->
    false;
does_user_exist(LUser, _LServer) ->
    case wocky_db:select_one(shared, user, user, #{user => LUser}) of
        not_found -> false;
        LUser -> true
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
    get_lookup(LUser, LServer, handle).


%% @doc Returns the user's phone number.
%%
%% `LUser': the "localpart" of the user's JID.
%%
%% `LServer': the "domainpart" of the user's JID.
%%
-spec get_phone_number(ejabberd:luser(), ejabberd:lserver())
                       -> phone_number() | {error, not_found}.
get_phone_number(LUser, LServer) ->
    get_lookup(LUser, LServer, phone_number).

%% @private
get_lookup(LUser, _LServer, Col) ->
    case wocky_db:select_one(shared, user, Col, #{user => LUser}) of
        not_found -> {error, not_found};
        null -> {error, not_found};
        Value -> Value
    end.


%% @doc Returns the user's password.
%%
%% `LUser': the "localpart" of the user's JID.
%%
%% `LServer': the "domainpart" of the user's JID.
%%
-spec get_password(ejabberd:luser(), ejabberd:lserver())
                  -> password() | {error, not_found}.
get_password(LUser, _LServer) ->
    case wocky_db:select_one(shared, user, password, #{user => LUser}) of
        not_found -> {error, not_found};
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
        true -> wocky_db:update(shared, user,
                                #{password => Password},
                                #{user => LUser})
    end.


%% @doc Removes the user from the database.
%%
%% `LUser': the "localpart" of the user's JID.
%%
%% `LServer': the "domainpart" of the user's JID.
%%
-spec remove_user(ejabberd:luser(), ejabberd:lserver()) -> ok.
remove_user(LUser, LServer) ->
    Columns = [handle, phone_number],
    case wocky_db:select_row(shared, user, Columns, #{user => LUser}) of
        not_found ->
            ok;

        #{handle := Handle, phone_number := PhoneNumber} ->
            %% TODO: this really needs to be done in a batch, but we don't
            %% currently have a clean way to run queries in different keyspaces
            %% in the same batch.
            ok = remove_handle_lookup(Handle),
            ok = remove_phone_lookup(PhoneNumber),
            ok = remove_user_record(LUser),
            ok = remove_tokens(LUser, LServer),
            ok
    end.

%% @private
remove_handle_lookup(null) -> ok;
remove_handle_lookup(Handle) ->
    wocky_db:delete(shared, handle_to_user, all, #{handle => Handle}).

%% @private
remove_phone_lookup(null) -> ok;
remove_phone_lookup(PhoneNumber) ->
    wocky_db:delete(shared, phone_number_to_user, all,
                    #{phone_number => PhoneNumber}).

%% @private
remove_user_record(LUser) ->
    wocky_db:delete(shared, user, all, #{user => LUser}).

%% @private
remove_tokens(LUser, LServer) ->
    wocky_db:delete(LServer, auth_token, all,
                    #{user => LUser, server => LServer}).


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
                  -> {ok, token(), pos_integer()} | {error, not_found}.
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
            {error, not_found}
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


%% @doc Returns `true' if a token is valid for the supplied
%% user/server/resource triplet, or `false' otherwise.
%%
%% `LUser': the "localpart" of the user's JID.
%%
%% `LServer': the "domainpart" of the user's JID.
%%
%% `Resource': the "resourcepart" of the user's JID.
%%
%% `Token': the token to test
%%
-spec check_token(ejabberd:lserver(), ejabberd:luser(),
                  ejabberd:lresource(), token()) -> boolean().
check_token(LUser, LServer, Resource, Token) ->
    Values = #{user => LUser,
               server => LServer,
               resource => Resource},
    case wocky_db:select_one(LServer, auth_token, auth_token, Values) of
        Token -> true;
        _ -> false
    end.


%% @doc Returns a map of all fields for a given user or `not_found' if no such
%% user exists.
%%
%% `LUser': the "localpart" of the user's JID.
%%
%% `LServer': the "domainpart" of the user's JID.
%%
-spec get_user_data(ejabberd:lserver(), ejabberd:lserver())
        -> map() | not_found.
get_user_data(LUser, _LServer) ->
    wocky_db:select_row(shared, user, all, #{user => LUser}).


%% @doc Returns the user ID associated with an authorization user name such
%% as that supplied by Tiwtter Digits.
%%
%% `LServer': the "domainpart" of the user's JID.
%%
%% `ExternalID': the authorization user name to look up.
%%
-spec get_user_by_external_id(ejabberd:lserver(), auth_name())
        -> ejabberd:luser() | not_found.
get_user_by_external_id(_LServer, ExternalID) ->
    wocky_db:select_one(shared, external_id, user,
                        #{external_id => ExternalID}).


%% @doc Returns the user ID and server associated with an given handle
%% or `not_found' if no such user exists.
%%
%% `Handle': the handle to look up.
%%
-spec get_user_by_handle(handle())
        -> {ejabberd:luser(), ejabberd:lserver()} | not_found.
get_user_by_handle(Handle) ->
    get_user_by_gkey(handle_to_user, handle, Handle).


%% @doc Returns the user ID and server associated with an given phone number
%% or `not_found' if no such user exists.
%%
%% `PhoneNumber': the phone number to look up.
%%
-spec get_user_by_phone_number(phone_number())
        -> {ejabberd:luser(), ejabberd:lserver()} | not_found.
get_user_by_phone_number(PhoneNumber) ->
    get_user_by_gkey(phone_number_to_user, phone_number, PhoneNumber).

%% @private
get_user_by_gkey(Table, Col, Value) ->
    case wocky_db:select_row(shared, Table, [user, server], #{Col => Value}) of
        #{user := User, server := Server} ->
            {User, Server};
        not_found ->
            not_found
    end.
