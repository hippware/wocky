-module(wocky_db_home_stream).

-compile({parse_transform, cut}).

-include_lib("ejabberd/include/jlib.hrl").
-include("wocky_publishing.hrl").

-export([
         publish/5,
         delete/3,
         get/2,
         get/3,
         get_catchup/3,
         current_version/2
        ]).


%%%===================================================================
%%% API
%%%===================================================================

-spec publish(ejabberd:luser(), ejabberd:lserver(), wocky_db:id(),
              published_stanza(), ejabberd:jid()) -> map().
publish(User, Server, ID, Item, From) ->
    delete_existing_item(User, Server, ID),
    ok = wocky_db:insert(Server, home_stream,
                         #{user     => User,
                           server   => Server,
                           id       => ID,
                           version  => now,
                           from_id  => jid:to_binary(From),
                           stanza   => exml:to_binary(Item),
                           deleted  => false
                          }),
    %% Read back the item for notification
    get(User, Server, ID).

-spec delete(ejabberd:luser(), ejabberd:lserver(), wocky_db:id()) -> map().
delete(User, Server, ID) ->
    delete_existing_item(User, Server, ID),
    ok = wocky_db:insert(Server, home_stream,
                         #{user     => User,
                           server   => Server,
                           id       => ID,
                           version  => now,
                           deleted  => true
                          }),
    %% Read back the item for notification
    get(User, Server, ID).

%% Get all items
-spec get(ejabberd:luser(), ejabberd:lserver()) -> [map()].
get(User, Server) ->
    Statement = <<"SELECT id, version, from_id, stanza, deleted FROM "
                  "home_stream_chronology WHERE user = ? AND server = ?">>,
    {ok, R} = wocky_db:query(Server, Statement,
                             #{user => User, server => Server},
                             quorum),
    [normalise_item(I) || I <- wocky_db:all_rows(R)].

%% Get a single item by ID
-spec get(ejabberd:luser(), ejabberd:lserver(), wocky_db:id()) ->
    map() | not_found.
get(User, Server, ID) ->
    normalise_item(
      wocky_db:select_row(Server, home_stream, all,
                          #{user => User, server => Server, id => ID})).

get_catchup(User, Server, Version) ->
    Statement = <<"SELECT id, version, from_id, stanza, deleted FROM "
                  "home_stream_chronology WHERE user = ? AND server = ? "
                  "AND version > ?">>,
    {ok, R} = wocky_db:query(Server,
                             Statement,
                             #{user => User,
                               server => Server,
                               version => Version},
                             quorum),
    [normalise_item(I) || I <- wocky_db:rows(R)].

-spec current_version(ejabberd:luser(), ejabberd:lserver()) -> pub_version().
current_version(User, Server) ->
    Statement = <<"SELECT MAX(version) FROM home_stream_chronology WHERE "
                  "USER = ? AND server = ?">>,
    {ok, R} = wocky_db:query(Server,
                             Statement,
                             #{user => User,
                               server => Server},
                             quorum),
    case wocky_db:single_result(R) of
        null -> not_found;
        X -> X
    end.

%%%===================================================================
%%% Helpers
%%%===================================================================

delete_existing_item(User, Server, ID) ->
    ok = wocky_db:delete(Server, home_stream, all,
                         #{user   => User,
                           server => Server,
                           id     => ID}).

normalise_item(not_found) -> not_found;
normalise_item(Item = #{deleted := true}) ->
    normalise_item2(Item, #xmlel{name = <<>>});
normalise_item(Item = #{stanza := StanzaBin}) ->
    {ok, Stanza} = wocky_xml:parse_multiple(StanzaBin),
    normalise_item2(Item, Stanza).

normalise_item2(#{id := ID, version := Version, from_id := From,
                 deleted := Deleted}, Stanza) ->
    #{id => ID,
      version => Version,
      from => safe_jid_from_binary(From),
      stanza => Stanza,
      deleted => Deleted}.

safe_jid_from_binary(null) -> #jid{};
safe_jid_from_binary(Bin) -> jid:from_binary(Bin).
