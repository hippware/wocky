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
        {ok, F} ->
            send_file(F, Req);
        not_found ->
            success(cowboy_req:reply(404, plain_header(), "Not found", Req))
    end.

send_file(File, Req) ->
    {File2, Data} = francus:read(File),
    #{<<"content-type">> := ContentType} = francus:metadata(File2),
    success(cowboy_req:reply(200,
                             [{<<"content-type">>, ContentType}],
                             Data, Req)).

do_put(Request = #hxep_request{request = {User, FileID, _},
                               user_server = Context,
                               metadata = Metadata =
                                          #{<<"content-type">> := ContentType}
                              }, Req) ->
    {ReqContentType, Req2} = cowboy_req:header(<<"content-type">>, Req, <<>>),
    case ReqContentType of
        ContentType ->
            {ok, F} = francus:open_write(Context, FileID, User, Metadata),
            write_data(F, Request, Req2);
        _ ->
            success(
              cowboy_req:reply(415, [],
                               "Mismatched media types between IQ and PUT",
                               Req2))
    end.

write_data(F, Request = #hxep_request{size = SizeRemaining}, Req) ->
    {Result, Data, NewSizeRemaining, Req2} = get_data(SizeRemaining, Req),
    Request2 = Request#hxep_request{size = NewSizeRemaining},
    case Result of
        more ->
            F2 = francus:write(F, Data),
            write_data(F2, Request2, Req2);
        ok ->
            F2 = francus:write(F, Data),
            finish_write(F2, Request2, Req2);
        excess_data ->
            finish_write(F, Request2, Req2)
    end.

get_data(SizeRemaining, Req) ->
    {Result, Data, Req2} = cowboy_req:body(Req),
    NewSizeRemaining = SizeRemaining - byte_size(Data),
    case NewSizeRemaining of
        X when X >= 0 -> {Result, Data, NewSizeRemaining, Req2};
        _ ->             {excess_data, <<>>, NewSizeRemaining, Req2}
    end.

% Exactly the right number of bytes received and written. All is well.
finish_write(F, #hxep_request{size = 0}, Req) ->
    ok = francus:close(F),
    success(cowboy_req:reply(200, Req));

% Too many or too few bytes received - close and delete the file and send an
% error code.
finish_write(F, #hxep_request{request = {_, File, _},
                              user_server = Context,
                              size = BytesRemaining}, Req) ->
    ok = francus:close(F),
    francus:delete(Context, File),
    case BytesRemaining of
        X when X > 0 ->
            success(cowboy_req:reply(400, [], "File was truncated", Req));
        X when X < 0 ->
            success(cowboy_req:reply(413, Req))
    end.

success({ok, Req}) -> Req.

-spec terminate({normal, shutdown} | {error, atom()}, cowboy_req:req(), any())
                -> ok.
terminate(_Reason, _Req, _State) -> ok.
