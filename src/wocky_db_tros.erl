%%% @copyright 2017+ Hippware, Inc.
%%% @doc DB interface for TROS file metadata
-module(wocky_db_tros).

-include("wocky.hrl").

-export([get_owner/2,
         get_access/2,
         set_access/3,
         get_metadata/2,
         set_metadata/4]).

-ignore_xref([get_access/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec get_owner(ejabberd:server(), wocky_db:id()) -> wocky_db:id() | not_found.
get_owner(Server, ID) ->
    wocky_db:select_one(shared, file_metadata, owner,
                        #{server => Server, id => ID}).

-spec get_access(ejabberd:server(), wocky_db:id()) -> binary() | not_found.
get_access(Server, ID) ->
    wocky_db:select_one(shared, file_metadata, access,
                        #{server => Server, id => ID}).

-spec set_access(ejabberd:server(), wocky_db:id(), binary()) -> ok.
set_access(Server, ID, Access) ->
    ok = wocky_db:insert(shared, file_metadata, #{id => ID,
                                                  server => Server,
                                                  access => Access}).

-spec get_metadata(ejabberd:server(), wocky_db:id()) -> map() | not_found.
get_metadata(Server, ID) ->
    wocky_db:select_row(shared, file_metadata, [owner, access],
                        #{id => ID, server => Server}).


-spec set_metadata(ejabberd:server(), wocky_db:id(), wocky_db:id(), binary())
-> ok.
set_metadata(Server, ID, Owner, Access) ->
    ok = wocky_db:insert(shared, file_metadata, #{id => ID,
                                                  server => Server,
                                                  access => Access,
                                                  owner => Owner}).
