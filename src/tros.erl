%%% @copyright 2016+ Hippware, Inc.
%%% @doc TROS helper functions
-module(tros).

-export([parse_url/1,
         make_jid/2,
         make_url/2
        ]).

-include_lib("ejabberd/include/jlib.hrl").

parse_url(<<"tros:", JID/binary>>) ->
    #jid{lserver = Server, lresource = Resource} = jid:from_binary(JID),
    case Resource of
        <<"file/", FileID/binary>> -> {ok, {Server, FileID}};
        _ -> {error, invalid_url}
    end;
parse_url(_) ->
    {error, invalid_url}.


make_jid(Server, FileID) ->
    jid:make(<<>>, Server, <<"file/", FileID/binary>>).

make_url(Server, FileID) ->
    <<"tros:", (jid:to_binary(make_jid(Server, FileID)))/binary>>.
