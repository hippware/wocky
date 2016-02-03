%%% @copyright 2016+ Hippware, Inc.
%%% @doc Wocky roster model
-module(wocky_db_roster).

-include_lib("ejabberd/include/mod_roster.hrl").

%% API
-export([get_roster/2,
         get_roster_version/2,
         get_roster_updates/3,
         delete_roster/2,
         get_roster_item/3,
         update_roster_item/4,
         delete_roster_item/3]).

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
    Query = "SELECT * FROM roster WHERE user = ?",
    {ok, R} = wocky_db:query(LServer, Query, #{user => LUser}, quorum),
    pack_roster(LUser, LServer, wocky_db:rows(R)).


%% @doc Returns the version of the given user's roster. If there are no roster
%% entries for the user, returns the null version of `0'.
-spec get_roster_version(ejabberd:luser(), ejabberd:lserver())
                        -> version().
get_roster_version(LUser, LServer) ->
    Query = "SELECT max(version) FROM roster WHERE user = ?",
    {ok, R} = wocky_db:query(LServer, Query, #{user => LUser}, quorum),
    Version = wocky_db:single_result(R, 0),
    integer_to_binary(Version).


%% @doc Returns all roster entries for the user that have a version higher
%% than the one specified. Returns an empty list if not roster items have a
%% higher version or if there are no roster items for the user.
-spec get_roster_updates(ejabberd:luser(), ejabberd:lserver(), version())
                        -> roster().
get_roster_updates(LUser, LServer, Version) ->
    Query = "SELECT * FROM roster_version WHERE user = ? AND version > ?",
    Values = #{user => LUser, version => binary_to_integer(Version)},
    {ok, R} = wocky_db:query(LServer, Query, Values, quorum),
    pack_roster(LUser, LServer, wocky_db:rows(R)).


%% @doc Deletes all roster items for the specified user.
-spec delete_roster(ejabberd:luser(), ejabberd:lserver()) -> ok.
delete_roster(LUser, LServer) ->
    Query = "DELETE FROM roster WHERE user = ?",
    {ok, void} = wocky_db:query(LServer, Query, #{user => LUser}, quorum),
    ok.


%% @doc Returns the roster item for the specified user and contact. If the user
%% does not have a roster item for the specified contact, returns a fresh
%% roster item that has not been stored in the database.
-spec get_roster_item(ejabberd:luser(), ejabberd:lserver(), contact())
                     -> roster_item().
get_roster_item(LUser, LServer, ContactJID) ->
    Query = "SELECT * FROM roster WHERE user = ? AND contact = ?",
    Values = #{user => LUser, contact => ContactJID},
    {ok, R} = wocky_db:query(LServer, Query, Values, quorum),
    pack_roster_item(LUser, LServer, ContactJID, wocky_db:single_row(R)).


%% @doc Stores the roster item in the database.
-spec update_roster_item(ejabberd:luser(), ejabberd:lserver(),
                         contact(), roster_item()) -> ok.
update_roster_item(LUser, LServer, ContactJID, Item) ->
    Query = "INSERT INTO roster ("
            "  user, server, contact, nick, groups,"
            "  ask, askmessage, subscription, version"
            ") VALUES (?, ?, ?, ?, ?, ?, ?, ?, toTimestamp(now()))",
    Values = unpack_roster_item(LUser, LServer, ContactJID, Item),
    {ok, void} = wocky_db:query(LServer, Query, Values, quorum),
    ok.


%% @doc Deletes the roster item from the database.
-spec delete_roster_item(ejabberd:luser(), ejabberd:lserver(), contact()) -> ok.
delete_roster_item(LUser, LServer, ContactJID) ->
    Query = "DELETE FROM roster WHERE user = ? AND contact = ?",
    Values = #{user => LUser, contact => ContactJID},
    {ok, void} = wocky_db:query(LServer, Query, Values, quorum),
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

pack_roster(LUser, LServer, Rows) ->
    [pack_roster_item(LUser, LServer, Row) || Row <- Rows].

pack_roster_item(LUser, LServer, #{contact := C} = Row) ->
    pack_roster_item(LUser, LServer, C, Row).

pack_roster_item(LUser, LServer, ContactJID, undefined) ->
    pack_roster_item(LUser, LServer, ContactJID, #{});
pack_roster_item(LUser, LServer, Contact, Row) when is_binary(Contact) ->
    ContactJID = jid:to_lower(jid:from_binary(Contact)),
    pack_roster_item(LUser, LServer, ContactJID, Row);
pack_roster_item(LUser, LServer, ContactJID, Row) ->
    #roster{
       usj          = {LUser, LServer, ContactJID},
       us           = {LUser, LServer},
       jid          = ContactJID,
       name         = maps:get(nick, Row, <<>>),
       groups       = maps:get(groups, Row, []),
       ask          = binary_to_atom(maps:get(ask, Row, <<"none">>), utf8),
       askmessage   = maps:get(askmessage, Row, <<>>),
       subscription = binary_to_atom(
                        maps:get(subscription, Row, <<"none">>), utf8)}.

unpack_roster_item(LUser, LServer, ContactJID, Item) ->
    #{user         => LUser,
      server       => LServer,
      contact      => ContactJID,
      nick         => Item#roster.name,
      groups       => Item#roster.groups,
      ask          => atom_to_binary(Item#roster.ask, utf8),
      askmessage   => Item#roster.askmessage,
      subscription => atom_to_binary(Item#roster.subscription, utf8)}.
