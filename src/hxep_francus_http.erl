-module(hxep_francus_http).

-include("mod_hxep_francus.hrl").

-export([init/3,
         handle/2,
         terminate/3]).

-spec init({atom(), http}, cowboy_req:req(), any())
            -> {ok, cowboy_req:req(), any()}.
init({_TransportName, http}, Req, _Opts) ->
    {ok, Req, none}.

-spec handle(cowboy_req:req(), any()) -> {ok, cowboy_req:req(), any()}.
handle(Req, State) ->
    Req3 = case cowboy_req:method(Req) of
        {<<"GET">>, Req2} -> handle_req(Req2);
        {<<"PUT">>, Req2} -> handle_req(Req2);
        {_, Req2} ->
                   success(cowboy_req:reply(405, plain_header(),
                                            "Unsupported method.", Req2))
    end,
    {ok, Req3, State}.

handle_req(Req) ->
    case passes_auth(Req) of
        {AR = #hxep_request{op = get}, Req2} -> do_get(AR, Req2);
        {AR = #hxep_request{op = put}, Req2} -> do_put(AR, Req2);
        {false, Req2} ->
            success(cowboy_req:reply(401, plain_header(),
                                     "Unauthorised request", Req2))
    end.

passes_auth(Req) ->
    {Path, Req2} = cowboy_req:path(Req),
    {User, File} = user_file(Path),
    {Auth, Req3} = cowboy_req:header(<<"authorization">>, Req2, <<>>),
    AuthReqKey = {User, File, Auth},
    AuthResult = hxep_req_tracker:get_delete(AuthReqKey),
    {AuthResult, Req3}.

plain_header() -> [{<<"content-type">>, <<"text/plain">>}].

user_file(<<$/, Path/binary>>) ->
    case binary:split(Path, <<$/>>, [global]) of
        [<<"users">>, User, <<"files">>, File] -> {User, File};
        _ -> {<<>>, <<>>}
    end.

do_get(#hxep_request{request = {_User, FileID, _},
                     user_server = Context}, Req) ->
    case francus:open_read(Context, FileID) of
        %% TODO: Chunked writes for larger files
        {ok, F} -> send_file(F, Req);
        not_found ->
            success(cowboy_req:reply(404, plain_header(), "Not found", Req))
    end.

send_file(File, Req) ->
    {File2, Data} = francus:read(File),
    success(cowboy_req:reply(200,
                             [{<<"content-type">>,
                               francus:content_type(File2)}],
                             Data, Req)).

do_put(#hxep_request{request = {User, FileID, _},
                     user_server = Context}, Req) ->
    {ContentType, Req2} = cowboy_req:header(<<"content-type">>, Req, <<>>),
    {ok, F} = francus:open_write(Context, FileID, User, ContentType),
    write_data(F, Req2).

write_data(F, Req) ->
    {More, Data, Req2} = cowboy_req:body(Req),
    F2 = francus:write(F, Data),
    case More of
        more ->
            write_data(F2, Req2);
        ok ->
            ok = francus:close(F2),
            success(cowboy_req:reply(200, Req2))
    end.

success({ok, Req}) -> Req.

-spec terminate({normal, shutdown} | {error, atom()}, cowboy_req:req(), any())
                -> ok.
terminate(_Reason, _Req, _State) -> ok.
