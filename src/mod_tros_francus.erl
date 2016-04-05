%%% @copyright 2016+ Hippware, Inc.
%%% @doc Francus backend for `mod_tros'
-module(mod_tros_francus).

-include_lib("ejabberd/include/jlib.hrl").
-include("mod_tros_francus.hrl").

-export([start/1,
         stop/0,
         make_download_response/4,
         make_upload_response/5,
         make_auth/0
        ]).

-define(DEFAULT_SCHEME, "https://").
-define(DEFAULT_AUTH_VALIDITY, 3600). % C* TTL in seconds
-define(DEFAULT_PORT, 1025).

configs() ->
    %% Name in .cfg   |Name in ejabberd_config|Default value
    [
     {scheme,          tros_scheme,           ?DEFAULT_SCHEME},
     {auth_validity,   tros_auth_validity,    ?DEFAULT_AUTH_VALIDITY},
     {port,            tros_port,             ?DEFAULT_PORT},
     {public_port,     tros_public_port,      ?DEFAULT_PORT}
    ].

start(Opts) ->
    lists:foreach(fun(C) -> mod_tros:set_config_from_opt(C, Opts) end,
                  configs()),
    tros_req_tracker:start(),
    Dispatch = cowboy_router:compile([{'_', [{'_', tros_francus_http, []}]}]),
    cowboy:start_http(tros_francus_listener, 100,
                       [
                        {port, port()}
                       ],
                       [{env, [{dispatch, Dispatch}]}]).

stop() ->
    tros_req_tracker:stop().

make_download_response(FromJID, ToJID, OwnerID, FileID) ->
    {Auth, _User, URL} =
        common_response_data(FromJID, ToJID, OwnerID, FileID),
    add_request(get, OwnerID, FileID, Auth, 0, #{}),
    Headers = [{<<"authorization">>, Auth}],
    RespFields = [
                  {<<"url">>, URL},
                  {<<"method">>, <<"GET">>}
                 ],
    {Headers, RespFields}.

make_upload_response(FromJID, ToJID, FileID, Size, Metadata =
                     #{<<"content-type">> := ContentType}) ->
    {Auth, User, URL} =
        common_response_data(FromJID, ToJID, FromJID#jid.luser, FileID),
    add_request(post, User, FileID, Auth, Size, Metadata),
    Headers = [
               {<<"content-type">>, ContentType},
               {<<"authorization">>, Auth}
              ],
    FileJID = jid:replace_resource(FromJID, <<"file/", FileID/binary>>),
    ReferenceURL = <<"tros:", (jid:to_binary(FileJID))/binary>>,
    RespFields = [
                  {<<"url">>, URL},
                  {<<"method">>, <<"POST">>},
                  {<<"referenceURL">>, ReferenceURL}],
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

add_request(Method, User, FileID, Auth, Size, Metadata) ->
    Req = #tros_request{method = Method, user = User, file = FileID,
                        auth = Auth, size = Size, metadata = Metadata
                       },
    tros_req_tracker:add(Req).

port() -> ejabberd_config:get_local_option(tros_port).
public_port() -> ejabberd_config:get_local_option(tros_public_port).

url(Server, User, FileID) ->
    Scheme = ejabberd_config:get_local_option(tros_scheme),
    iolist_to_binary(
      [Scheme, Server, ":", integer_to_list(public_port()),
       "/users/", User, "/files/", FileID]).
