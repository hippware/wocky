-module(mod_hxep_francus).

-include_lib("ejabberd/include/jlib.hrl").
-include("mod_hxep_francus.hrl").

-export([start/1,
         stop/0,
         make_download_response/3,
         make_upload_response/4,
         make_auth/0
        ]).

-define(DEFAULT_PORT, 1025).

start(_Opts) ->
    hxep_req_tracker:start(),
    Dispatch = cowboy_router:compile([{'_', [{'_', hxep_francus_http, []}]}]),
    cowboy:start_https(hxep_francus_listener, 100,
                       [
                        {port, port()},
                        {cacertfile, ssl_file("fake_cert.pem")},
                        {certfile, ssl_file("fake_server.pem")},
                        {keyfile, ssl_file("fake_key.pem")}
                       ],
                       [{env, [{dispatch, Dispatch}]}]).

ssl_file(Filename) ->
    PrivDir = code:priv_dir(wocky),
    filename:join([PrivDir, "ssl", Filename]).

stop() ->
    hxep_req_tracker:stop().

make_download_response(FromJID, ToJID, FileID) ->
    {Auth, User, UserServer, URL} =
        common_response_data(FromJID, ToJID, FileID),
    add_request(get, User, FileID, UserServer, Auth),
    Headers = [{<<"authorization">>, Auth}],
    RespFields = [
                  {<<"url">>, URL},
                  {<<"method">>, <<"GET">>}
                 ],
    {Headers, RespFields}.

make_upload_response(FromJID, ToJID, FileID, MimeType) ->
    {Auth, User, UserServer, URL} =
        common_response_data(FromJID, ToJID, FileID),
    add_request(put, User, FileID, UserServer, Auth),
    Headers = [
               {<<"content-type">>, MimeType},
               {<<"authorization">>, Auth}
              ],
    RespFields = [
                  {<<"url">>, URL},
                  {<<"method">>, <<"PUT">>}
                 ],
    {Headers, RespFields}.

common_response_data(FromJID, ToJID, FileID) ->
    %% Explicit module named added to allow us to mock out make_auth/0 for
    %% testing
    Auth = ?MODULE:make_auth(),
    User = FromJID#jid.luser,
    UserServer = FromJID#jid.lserver,
    Server = ToJID#jid.lserver,
    URL = url(Server, User, FileID),
    {Auth, User, UserServer, URL}.

make_auth() ->
    base64:encode(crypto:strong_rand_bytes(128)).

add_request(Op, User, FileID, UserServer, Auth) ->
    Req = #hxep_request{op = Op, request = {User, FileID, Auth},
                        user_server = UserServer},
    hxep_req_tracker:add(Req).

port() -> ?DEFAULT_PORT.

url(Server, User, FileID) ->
    iolist_to_binary(
      ["https://", Server, ":", integer_to_list(port()),
       "/users/", User, "/files/", FileID]).
