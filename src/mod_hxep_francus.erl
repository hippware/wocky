-module(mod_hxep_francus).

-include_lib("ejabberd/include/jlib.hrl").
-include("mod_hxep_francus.hrl").

-export([start/0,
         stop/0,
         make_download_response/3,
         make_upload_response/4,
         expire_request/1,

         % Exports for testing only:
         make_auth/0
        ]).

-define(DEFAULT_PORT, 1025).
-define(DEFAULT_TIMEOUT, timer:seconds(60)).

start() ->
    hxep_requests = ets:new(hxep_requests, [named_table, public,
                                            {keypos, #hxep_request.request}]),
    Dispatch = cowboy_router:compile([{'_', [{'_', hxep_francus_http, []}]}]),
    cowboy:start_http(hxep_francus_listener, 100,
                      [{port, port()}],
                      [{env, [{dispatch, Dispatch}]}]).

stop() ->
    ets:delete(hxep_requests).

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
    % Explicit module named added to allow us to mock out make_auth/0 for
    % testing
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
    {ok, TRef} = timer:apply_after(timeout(), ?MODULE, expire_request,
                                   [Req#hxep_request.request]),
    ets:insert(hxep_requests, Req#hxep_request{tref = TRef}).

port() -> ?DEFAULT_PORT.
timeout() -> ?DEFAULT_TIMEOUT.

expire_request(Request) ->
    ets:delete(hxep_requests, Request).

url(Server, User, FileID) ->
    iolist_to_binary(
      ["http://", Server, ":", integer_to_list(port()),
       "/", User, "/", FileID]).
