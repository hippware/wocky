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
-type token() :: binary().
-export_type([handle/0, password/0, token/0]).

%% API
-export([create_id/0,
         normalize_id/1,
         is_valid_id/1,
         create_user/3,
         create_user/4,
         does_user_exist/2,
         get_handle/2,
         get_password/2,
         set_password/3,
         remove_user/2,
         generate_token/0,
         assign_token/3,
         release_token/3,
         get_tokens/2]).

-define(TOKEN_BYTES, 32).
-define(TOKEN_MARKER, "$T$").
-define(TOKEN_EXPIRE, 1209600). % two weeks in seconds


%%%===================================================================
%%% API
%%%===================================================================

%% @doc Generates a timeuuid in canonical text format for use as a user id.
-spec create_id() -> ejabberd:luser().
create_id() ->
    ossp_uuid:make(v1, text).


%% @doc Takes a raw binary UUID (as retrieved through C* queries) and converts
%% to the cannonical textual binary UUID form.
-spec normalize_id(binary()) -> ejabberd:luser().
normalize_id(UUID) ->
    ossp_uuid:import(UUID, text).


%% @doc Returns true if the user ID is a valid UUID.
-spec is_valid_id(ejabberd:luser()) -> boolean().
is_valid_id(LUser) ->
    try
        ossp_uuid:import(LUser, binary),
        true
    catch
        _:_ ->
            false
    end.


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
                 -> ok | {error, exists | invalid_id}.
create_user(LUser, LServer, Handle, Password) ->
    create_user(LUser, LServer, Handle, Password, is_valid_id(LUser)).

%% @private
create_user(LUser, LServer, Handle, Password, true) ->
    %% TODO: this really needs to be done in a batch, but we don't currently
    %% have a clean way to run queries in different keyspaces in the same batch.
    case create_handle_lookup(LUser, LServer, Handle) of
        true ->
            create_user_record(LUser, LServer, Handle, Password);

        false ->
            {error, exists}
    end;
create_user(_, _, _, _, false) ->
    {error, invalid_id}.

%% @private
create_handle_lookup(LUser, LServer, Handle) ->
    Values = #{user => LUser, server => LServer, handle => Handle},
    wocky_db:insert_new(shared, handle_to_user, Values).

%% @private
create_user_record(LUser, LServer, Handle, Password) ->
    Values = #{user => LUser, server => LServer,
               handle => Handle, password => Password},
    wocky_db:insert(LServer, user, Values).


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
    get_handle(LUser, LServer, is_valid_id(LUser)).

%% @private
get_handle(LUser, LServer, true) ->
    case wocky_db:select_one(LServer, user, handle, #{user => LUser}) of
        not_found -> {error, not_found};
        Handle -> Handle
    end;
get_handle(_, _, false) ->
    {error, not_found}.


%% @doc Returns the user's password.
%%
%% `LUser': the "localpart" of the user's JID.
%%
%% `LServer': the "domainpart" of the user's JID.
%%
-spec get_password(ejabberd:luser(), ejabberd:lserver())
                  -> password() | {error, not_found}.
get_password(LUser, LServer) ->
    get_password(LUser, LServer, is_valid_id(LUser)).

%% @private
get_password(LUser, LServer, true) ->
    case wocky_db:select_one(LServer, user, password, #{user => LUser}) of
        not_found -> {error, not_found};
        Password -> Password
    end;
get_password(_, _, false) ->
    {error, not_found}.


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
        true -> wocky_db:update(LServer, user,
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
    case get_handle(LUser, LServer) of
        {error, not_found} ->
            ok;

        Handle ->
            %% TODO: this really needs to be done in a batch, but we don't
            %% currently have a clean way to run queries in different keyspaces
            %% in the same batch.
            ok = remove_handle_lookup(Handle),
            ok = remove_user_record(LUser, LServer),
            ok
    end.

%% @private
remove_handle_lookup(Handle) ->
    wocky_db:delete(shared, handle_to_user, all, #{handle => Handle}).

%% @private
remove_user_record(LUser, LServer) ->
    wocky_db:delete(LServer, user, all, #{user => LUser}).


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
                  -> {ok, token()} | {error, not_found}.
assign_token(LUser, LServer, LResource) ->
    case does_user_exist(LUser, LServer) of
        true ->
            Token = generate_token(),
            ok = wocky_db:insert(LServer, auth_token,
                                 #{user => LUser,
                                   server => LServer,
                                   resource => LResource,
                                   auth_token => Token,
                                   '[ttl]' => ?TOKEN_EXPIRE}),
            {ok, Token};

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
    ok = release_token(LUser, LServer, LResource, is_valid_id(LUser)).

%% @private
release_token(LUser, LServer, LResource, true) ->
    wocky_db:delete(LServer, auth_token, all, #{user => LUser,
                                                server => LServer,
                                                resource => LResource});
release_token(_, _, _, false) ->
    ok.


%% @doc Returns all tokens currently assigned to resources belonging to the
%% specified user.
%%
%% `LUser': the "localpart" of the user's JID.
%%
%% `LServer': the "domainpart" of the user's JID.
%%
-spec get_tokens(ejabberd:luser(), ejabberd:lserver()) -> [token()].
get_tokens(LUser, LServer) ->
    Rows = get_tokens(LUser, LServer, is_valid_id(LUser)),
    [Token || #{auth_token := Token} <- Rows].

%% @private
get_tokens(LUser, LServer, true) ->
    wocky_db:select(LServer, auth_token, [auth_token],
                    #{user => LUser, server => LServer});
get_tokens(_, _, false) ->
    [].
