%%% @copyright 2016+ Hippware, Inc.
%%% @doc Wocky roster model
-module(wocky_db_roster).

-include("wocky_roster.hrl").
-include_lib("ejabberd/include/jlib.hrl").

%% API
-export([get_roster/2,
         get_roster_version/2,
         delete_roster/2,
         get_roster_item/3,
         update_roster_item/4,
         bump_roster_version/2,
         delete_roster_item/3,
         has_contact/2,
         is_friend/2,
         is_friend/1,
         users_with_contact/1
        ]).

-type roster()      :: [wocky_roster()].
-type version()     :: binary().
-type contact()     :: binary().
-export_type([roster/0, version/0]).


%%%===================================================================
%%% API
%%%===================================================================

%% @doc Returns the roster for the given user in the form of a list of
%% roster entries, or an empty list if no entries were found for the user.
-spec get_roster(ejabberd:luser(), ejabberd:lserver()) -> roster().
get_roster(LUser, LServer) ->
    Rows = wocky_db:select(shared, roster, all, #{user => LUser}),
    Items = pack_roster(LUser, LServer, Rows),
    fill_extra_fields(Items).


%% @doc Returns the version of the given user's roster. If there are no roster
%% entries for the user, returns the null version of `0'.
-spec get_roster_version(ejabberd:luser(), ejabberd:lserver())
                        -> version().
get_roster_version(LUser, _LServer) ->
    Result = wocky_db:select_row(shared, roster,
                                 ['max(version)', 'count(version)'],
                                 #{user => LUser}),
    case Result of
        #{'system.max(version)' := null} ->
            make_version(0, 0);
        #{'system.max(version)' := V, 'system.count(version)' := C} ->
            make_version(V, C)
    end.

make_version(Version, Count) ->
    <<(integer_to_binary(Version))/binary, $-,
      (integer_to_binary(Count))/binary>>.

%% @doc Deletes all roster items for the specified user.
-spec delete_roster(ejabberd:luser(), ejabberd:lserver()) -> ok.
delete_roster(LUser, _LServer) ->
    wocky_db:delete(shared, roster, all, #{user => LUser}).


%% @doc Returns the roster item for the specified user and contact. If the user
%% does not have a roster item for the specified contact, returns a fresh
%% roster item that has not been stored in the database.
-spec get_roster_item(ejabberd:luser(), ejabberd:lserver(), contact())
                     -> wocky_roster().
get_roster_item(LUser, LServer, ContactJID) ->
    Conditions = #{user => LUser, contact_jid => ContactJID},
    Row = wocky_db:select_row(shared, roster, all, Conditions),
    Item = pack_roster_item(LUser, LServer, ContactJID, Row),
    fill_extra_fields(Item).


%% @doc Stores the roster item in the database.
-spec update_roster_item(ejabberd:luser(), ejabberd:lserver(),
                         contact(), wocky_roster()) -> ok.
update_roster_item(LUser, LServer, ContactJID, Item) ->
    Query = "INSERT INTO roster ("
            " user, server, contact_jid, nick, groups, ask,"
            " subscription, version"
            ") VALUES (?, ?, ?, ?, ?, ?, ?, toTimestamp(now()))",
    Values = unpack_roster_item(LUser, LServer, ContactJID, Item),
    {ok, void} = wocky_db:query(shared, Query, Values, quorum),
    ok.

%% @doc Updates the roster version on the specified entry without
%% changing any other data.
-spec bump_roster_version(ejabberd:luser(), contact()) -> ok.
bump_roster_version(LUser, ContactJID) ->
    Query = "UPDATE roster SET version = toTimestamp(now())"
            " WHERE user = ? AND contact_jid = ?",
    Values = #{user => LUser,
               contact_jid => ContactJID},
    {ok, void} = wocky_db:query(shared, Query, Values, quorum),
    ok.


%% @doc Deletes the roster item from the database.
-spec delete_roster_item(ejabberd:luser(), ejabberd:lserver(), contact()) -> ok.
delete_roster_item(LUser, _LServer, ContactJID) ->
    Conditions = #{user => LUser, contact_jid => ContactJID},
    wocky_db:delete(shared, roster, all, Conditions).

%% @doc Checks whether a user has another user as an authorized contact
-spec has_contact(ejabberd:jid(), ejabberd:jid()) -> boolean().
has_contact(#jid{luser = LUser}, OtherJID) ->
    ContactJID = jid:to_binary(jid:to_bare(OtherJID)),
    case wocky_db:select(shared, roster, [ask],
                         #{user => LUser,
                           contact_jid => ContactJID}) of
        [#{ask := <<"none">>}] -> true;
        _ -> false
    end.

%% @doc Checks whether a user has another user as a friend
-spec is_friend(ejabberd:jid(), ejabberd:jid()) -> boolean().
is_friend(#jid{luser = LUser}, OtherJID) ->
    ContactJID = jid:to_binary(jid:to_bare(OtherJID)),
    case wocky_db:select(shared, roster, [subscription, groups],
                         #{user => LUser,
                           contact_jid => ContactJID}) of
        [#{subscription := Sub, groups := Groups}] ->
            wocky_util:is_friend(binary_to_atom(Sub, utf8), Groups);
        _ ->
            false
    end.

-spec is_friend(wocky_roster()) -> boolean().
is_friend(#wocky_roster{subscription = Sub, groups = Groups}) ->
    wocky_util:is_friend(Sub, Groups).

-spec users_with_contact(ejabberd:jid()) -> [ejabberd:jid()].
users_with_contact(ContactJID) ->
    ContactJIDBin = jid:to_binary(jid:to_bare(ContactJID)),
    Rows = wocky_db:select(shared, reverse_roster, [user, server],
                           #{contact_jid => ContactJIDBin}),
    [jid:make(U, S, <<>>) || #{user := U, server := S} <- Rows].

%%%===================================================================
%%% Internal functions
%%%===================================================================

pack_roster(LUser, LServer, Rows) ->
    [pack_roster_item(LUser, LServer, Row) || Row <- Rows].

pack_roster_item(LUser, LServer, #{contact_jid := C} = Row) ->
    pack_roster_item(LUser, LServer, C, Row).

pack_roster_item(LUser, LServer, ContactJID, not_found) ->
    pack_roster_item(LUser, LServer, ContactJID, #{});
pack_roster_item(LUser, LServer, Contact, Row) when is_binary(Contact) ->
    ContactJID = jid:to_lower(jid:from_binary(Contact)),
    pack_roster_item(LUser, LServer, ContactJID, Row);
pack_roster_item(LUser, LServer, ContactJID, Row0) ->
    Row = wocky_db:drop_nulls(Row0),
    #wocky_roster{
       user           = LUser,
       server         = LServer,
       contact_jid    = ContactJID,
       name           = maps:get(nick, Row, <<>>),
       groups         = maps:get(groups, Row, []),
       ask            = binary_to_atom(maps:get(ask, Row, <<"none">>), utf8),
       subscription   = binary_to_atom(
                          maps:get(subscription, Row, <<"none">>), utf8)}.

fill_extra_fields(Items) when is_list(Items) ->
    [fill_extra_fields(Item) || Item <- Items];

fill_extra_fields(#wocky_roster{contact_jid = {LUser, LServer, _}} = I) ->
    case wocky_db_user:find_user(LUser, LServer) of
        not_found ->
            I;

        #{handle := Handle, avatar := Avatar,
          first_name := First, last_name := Last} ->
            I#wocky_roster{
              avatar = safe_value(Avatar),
              contact_handle = safe_value(Handle),
              first_name = safe_value(First),
              last_name = safe_value(Last)
             }
    end.

safe_value(null) -> <<>>;
safe_value(Value) -> Value.

unpack_roster_item(LUser, LServer, ContactJID, Item) ->
    #{user           => LUser,
      server         => LServer,
      contact_jid    => ContactJID,
      nick           => Item#wocky_roster.name,
      groups         => Item#wocky_roster.groups,
      ask            => atom_to_binary(Item#wocky_roster.ask, utf8),
      subscription   => atom_to_binary(Item#wocky_roster.subscription, utf8)}.
