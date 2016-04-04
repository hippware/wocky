%%% @copyright 2016+ Hippware, Inc.
%%% @doc Wocky roster model
-module(wocky_db_roster).

-include("wocky_roster.hrl").
-include_lib("ejabberd/include/jlib.hrl").

%% API
-export([get_roster/2,
         get_roster_version/2,
         get_roster_updates/3,
         delete_roster/2,
         get_roster_item/3,
         update_roster_item/4,
         delete_roster_item/3,
         has_contact/2]).

-type roster_item() :: #roster{}.
-type roster()      :: [roster_item()].
-type version()     :: binary().
-type contact()     :: binary().
-export_type([roster_item/0, roster/0, version/0]).


%%%===================================================================
%%% API
%%%===================================================================

%% @doc Returns the roster for the given user in the form of a list of
%% roster entries, or an empty list if no entries were found for the user.
-spec get_roster(ejabberd:luser(), ejabberd:lserver()) -> roster().
get_roster(LUser, LServer) ->
    Rows = wocky_db:select(LServer, roster, all, #{user => LUser}),
    Items = pack_roster(LUser, LServer, Rows),
    fill_extra_fields(Items).


%% @doc Returns the version of the given user's roster. If there are no roster
%% entries for the user, returns the null version of `0'.
-spec get_roster_version(ejabberd:luser(), ejabberd:lserver())
                        -> version().
get_roster_version(LUser, LServer) ->
    Value = wocky_db:select_one(LServer, roster, 'max(version)',
                                #{user => LUser}),
    case Value of
        null -> <<"0">>;
        Version -> integer_to_binary(Version)
    end.


%% @doc Returns all roster entries for the user that have a version higher
%% than the one specified. Returns an empty list if not roster items have a
%% higher version or if there are no roster items for the user.
-spec get_roster_updates(ejabberd:luser(), ejabberd:lserver(), version())
                        -> roster().
get_roster_updates(LUser, LServer, Version) ->
    Query = "SELECT * FROM roster_version WHERE user = ? AND version > ?",
    Values = #{user => LUser, version => binary_to_integer(Version)},
    {ok, R} = wocky_db:query(LServer, Query, Values, quorum),
    Items = pack_roster(LUser, LServer, wocky_db:rows(R)),
    fill_extra_fields(Items).


%% @doc Deletes all roster items for the specified user.
-spec delete_roster(ejabberd:luser(), ejabberd:lserver()) -> ok.
delete_roster(LUser, LServer) ->
    wocky_db:delete(LServer, roster, all, #{user => LUser}).


%% @doc Returns the roster item for the specified user and contact. If the user
%% does not have a roster item for the specified contact, returns a fresh
%% roster item that has not been stored in the database.
-spec get_roster_item(ejabberd:luser(), ejabberd:lserver(), contact())
                     -> roster_item().
get_roster_item(LUser, LServer, ContactJID) ->
    Conditions = #{user => LUser, contact_jid => ContactJID},
    Row = wocky_db:select_row(LServer, roster, all, Conditions),
    Item = pack_roster_item(LUser, LServer, ContactJID, Row),
    fill_extra_fields(Item).


%% @doc Stores the roster item in the database.
-spec update_roster_item(ejabberd:luser(), ejabberd:lserver(),
                         contact(), roster_item()) -> ok.
update_roster_item(LUser, LServer, ContactJID, Item) ->
    Query = "INSERT INTO roster ("
            " user, server, contact_jid, nick, groups, ask, ask_message,"
            " subscription, version"
            ") VALUES (?, ?, ?, ?, ?, ?, ?, ?, toTimestamp(now()))",
    Values = unpack_roster_item(LUser, LServer, ContactJID, Item),
    {ok, void} = wocky_db:query(LServer, Query, Values, quorum),
    ok.


%% @doc Deletes the roster item from the database.
-spec delete_roster_item(ejabberd:luser(), ejabberd:lserver(), contact()) -> ok.
delete_roster_item(LUser, LServer, ContactJID) ->
    Conditions = #{user => LUser, contact_jid => ContactJID},
    wocky_db:delete(LServer, roster, all, Conditions).

%% @doc Checks whether a user has another user as an authorized contact
-spec has_contact(ejabberd:jid(), ejabberd:jid()) -> boolean().
has_contact(#jid{luser = LUser, lserver = LServer}, OtherJID) ->
    ContactJID = jid:to_binary(jid:to_bare(OtherJID)),
    case wocky_db:select(LServer, roster, [ask],
                         #{user => LUser,
                           contact_jid => ContactJID}) of
        [#{ask := <<"none">>}] -> true;
        _ -> false
    end.

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
    #roster{
       user           = LUser,
       server         = LServer,
       contact_jid    = ContactJID,
       name           = maps:get(nick, Row, <<>>),
       groups         = maps:get(groups, Row, []),
       ask            = binary_to_atom(maps:get(ask, Row, <<"none">>), utf8),
       ask_message    = maps:get(ask_message, Row, <<>>),
       subscription   = binary_to_atom(
                          maps:get(subscription, Row, <<"none">>), utf8)}.

fill_extra_fields(Items) when is_list(Items) ->
    [fill_extra_fields(Item) || Item <- Items];

fill_extra_fields(#roster{contact_jid = {LUser, _, _}} = I) ->
    Row = wocky_db:select_row(shared, user,
                              [handle, avatar, first_name, last_name],
                              #{user => LUser}),
    case Row of
        not_found ->
            I;

        #{handle := Handle, avatar := Avatar,
          first_name := First, last_name := Last} ->
            I#roster{
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
      nick           => Item#roster.name,
      groups         => Item#roster.groups,
      ask            => atom_to_binary(Item#roster.ask, utf8),
      ask_message    => Item#roster.ask_message,
      subscription   => atom_to_binary(Item#roster.subscription, utf8)}.
