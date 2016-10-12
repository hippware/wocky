%%% @copyright 2016+ Hippware, Inc.
%%% @doc Francus backend for `mod_wocky_tros'
-module(mod_wocky_tros_francus).

-include_lib("ejabberd/include/jlib.hrl").
-include("mod_wocky_tros_francus.hrl").

-behaviour(mod_wocky_tros_backend).

-export([start/1,
         stop/0,
         make_download_response/5,
         make_upload_response/6,
         make_auth/0,
         get_owner/1,
         get_access/1,
         get_metadata/2
        ]).

-define(DEFAULT_SCHEME, "https://").
-define(DEFAULT_AUTH_VALIDITY, 3600). % C* TTL in seconds
-define(DEFAULT_PORT, 1025).

-ignore_xref([get_access/1]).

configs() ->
    %% Name in .cfg   |Name in ejabberd_config|Default value
    [
     {scheme,          tros_scheme,           ?DEFAULT_SCHEME},
     {auth_validity,   tros_auth_validity,    ?DEFAULT_AUTH_VALIDITY},
     {port,            tros_port,             ?DEFAULT_PORT},
     {public_port,     tros_public_port,      ?DEFAULT_PORT}
    ].

start(Opts) ->
    lists:foreach(fun({Tag, Config, Default}) ->
                          wocky_util:set_config_from_opt(
                            Tag, Config, Default, Opts)
                  end, configs()),
    tros_req_tracker:start(),
    Dispatch = cowboy_router:compile([{'_', [{'_', tros_francus_http, []}]}]),
    {ok, _} = cowboy:start_http(tros_francus_listener, 100,
                                [
                                 {port, port()}
                                ],
                                [{env, [{dispatch, Dispatch}]}]),
    setup_metrics().

stop() ->
    _ = cowboy:stop_listener(tros_francus_listener),
    tros_req_tracker:stop().

make_download_response(FromJID, ToJID, OwnerID, FileID,
                       #{<<"name">> := Name}) ->
    {Auth, _User, URL} =
        common_response_data(FromJID, ToJID, OwnerID, FileID),
    add_request(get, OwnerID, FileID, Auth, 0, <<>>, #{}),
    Headers = [{<<"authorization">>, Auth}],
    EncodedFile = http_uri:encode(binary_to_list(Name)),
    RespFields = [
                  {<<"url">>, [URL, "/", EncodedFile]},
                  {<<"method">>, <<"GET">>}
                 ],
    {Headers, RespFields}.

make_upload_response(FromJID, ToJID, FileID, Size, Access,
                     Metadata = #{<<"content-type">> := ContentType}) ->
    {Auth, User, URL} =
        common_response_data(FromJID, ToJID, FromJID#jid.luser, FileID),
    add_request(post, User, FileID, Auth, Size, Access, Metadata),
    Headers = [
               {<<"content-type">>, ContentType},
               {<<"authorization">>, Auth}
              ],
    FileJID = jid:replace_resource(FromJID, <<"file/", FileID/binary>>),
    ReferenceURL = <<"tros:", (jid:to_binary(FileJID))/binary>>,
    RespFields = [
                  {<<"url">>, URL},
                  {<<"method">>, <<"POST">>},
                  {<<"reference_url">>, ReferenceURL}],
    {Headers, RespFields}.

common_response_data(FromJID, ToJID, Owner, FileID) ->
    %% Explicit module named added to allow us to mock out make_auth/0 for
    %% testing
    Auth = ?MODULE:make_auth(),
    User = FromJID#jid.luser,
    Server = ToJID#jid.lserver,
    URL = url(Server, Owner, FileID),
    {Auth, User, URL}.

make_auth() ->
    base64:encode(crypto:strong_rand_bytes(48)).

add_request(Method, User, FileID, Auth, Size, Access, Metadata) ->
    Req = #tros_request{method = Method, user = User, file = FileID,
                        auth = Auth, size = Size,
                        access = Access, metadata = Metadata
                       },
    tros_req_tracker:add(Req).

port() -> ejabberd_config:get_local_option(tros_port).
public_port() -> ejabberd_config:get_local_option(tros_public_port).

url(Server, User, FileID) ->
    Scheme = ejabberd_config:get_local_option(tros_scheme),
    iolist_to_binary(
      [Scheme, Server, ":", integer_to_list(public_port()),
       "/users/", User, "/files/", FileID]).

get_metadata(Server, FileID) ->
    case francus:open_read(Server, FileID) of
        {error, E} ->
            {error, E};
        {ok, File} ->
            Info = francus:metadata(File),
            Owner = francus:owner(File),
            Access = francus:access(File),
            francus:close(File),
            {ok, Info#{<<"owner">> => Owner,
                       <<"access">> => Access}}
    end.

get_owner(#{<<"owner">> := Owner}) -> {ok, Owner}.

get_access(#{<<"access">> := Access}) -> {ok, Access}.

setup_metrics() ->
    Metrics = [tros_francus_bytes_sent,
               tros_francus_bytes_received],
    wocky_metrics:setup_spiral(Metrics).
