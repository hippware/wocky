-module(wocky_db_tros).

-include("wocky.hrl").

-export([get_owner/2,
         get_access/2,
         set_access/3,
         set_metadata/4]).

%%%===================================================================
%%% API
%%%===================================================================

get_owner(Server, ID) ->
    wocky_db:select_one(shared, file_metadata, owner,
                        #{server => Server, id => ID}).

get_access(Server, ID) ->
    wocky_db:select_one(shared, file_metadata, access,
                        #{server => Server, id => ID}).

set_access(Server, ID, Access) ->
    ok = wocky_db:insert(shared, file_metadata, #{id => ID,
                                                  server => Server,
                                                  access => Access}).

set_metadata(Server, ID, Owner, Access) ->
    ok = wocky_db:insert(shared, file_metadata, #{id => ID,
                                                  server => Server,
                                                  access => Access,
                                                  owner => Owner}).
