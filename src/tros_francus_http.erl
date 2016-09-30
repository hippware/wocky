%%% @copyright 2016+ Hippware, Inc.
%%% @doc Cowboy HTTP server listener for TROS-Francus interface
-module(tros_francus_http).

-include("mod_wocky_tros_francus.hrl").

-compile({parse_transform, do}).

-export([init/3,
         handle/2,
         terminate/3]).

-ignore_xref([init/3, terminate/3, handle/2]).


-spec init({atom(), http}, cowboy_req:req(), any())
            -> {ok, cowboy_req:req(), any()}.
init({_TransportName, http}, Req, _Opts) ->
    {ok, Req, none}.

-spec handle(cowboy_req:req(), any()) -> {ok, cowboy_req:req(), any()}.
handle(Req, State) ->
    Req4 = case handle_req(Req) of
        {ok, Req2} ->
            Req2;
        {error, {Code, Msg, Req2}} ->
            {ok, Req3} = cowboy_req:reply(Code, plain_header(), Msg, Req2),
            Req3
    end,
    {ok, Req4, State}.

handle_req(Req) ->
    do([error_m ||
        {Method, Req1} <- check_method(Req),
        {User, File, Req2} <- check_path(Req1),
        {AuthReq, Req3} <- check_auth(User, File, Method, Req2),
        do_op(Method, AuthReq, Req3)]).

check_method(Req) ->
    case cowboy_req:method(Req) of
        {<<"GET">>, Req2} -> {ok, {get, Req2}};
        {<<"POST">>, Req2} -> {ok, {post, Req2}};
        {_, Req2} -> {error, {405, "Unsupported method", Req2}}
    end.

check_path(Req) ->
    {Path, Req2} = cowboy_req:path(Req),
    case user_file(Path) of
        {<<>>, <<>>} -> {error, {404, "Not found", Req2}};
        {User, File} -> {ok, {User, File, Req2}}
    end.

check_auth(User, File, Method, Req) ->
    {Auth, Req2} = cowboy_req:header(<<"authorization">>, Req, <<>>),
    case tros_req_tracker:check(User, File, Auth, Method) of
        false -> {error, {401, "Unauthorised request", Req2}};
        AuthReq -> {ok, {AuthReq, Req2}}
    end.

plain_header() -> [{<<"content-type">>, <<"text/plain">>}].

user_file(<<$/, Path/binary>>) ->
    case binary:split(Path, <<$/>>, [global]) of
        % Download tacks on the original filename at the end to help
        % client libraries (React) figure out the type - discard it:
        [<<"users">>, User, <<"files">>, File, _OriginalName] -> {User, File};
        % Uploads don't bother:
        [<<"users">>, User, <<"files">>, File] -> {User, File};
        _ -> {<<>>, <<>>}
    end;
user_file(_) ->
    {<<>>, <<>>}.

do_op(get, #tros_request{file = FileID}, Req) ->
    case francus:open_read(wocky_app:server(), FileID) of
        %% TODO: Chunked writes for larger files?
        {ok, F} ->
            send_file(F, Req);
        {error, not_found} ->
            {error, {404, "Not found", Req}}
    end;

do_op(post,
      Request = #tros_request{metadata = #{<<"content-type">> := ContentType}},
      Req) ->
    {ReqContentType, Req2} = cowboy_req:header(<<"content-type">>, Req, <<>>),
    case ReqContentType of
        ContentType ->
            open_and_write(Request, fun cowboy_req:body/1, Req2);
        <<"multipart/form-data", _/binary>> ->
            handle_multipart(Request, ContentType, Req2);
        _ ->
            {error, {415, "Mismatched media types between IQ and POST", Req2}}
    end.

handle_multipart(Request, ReqContentType, Req) ->
    case cowboy_req:part(Req) of
        {ok, Headers, Req2} ->
            process_part_headers(Request, ReqContentType, Headers, Req2);
        {done, Req2} ->
            {error, {400, "Could not find file part", Req2}}
    end.

process_part_headers(Request, ReqContentType, Headers, Req) ->
    case lists:keyfind(<<"content-type">>, 1, Headers) of
        {_, ReqContentType} ->
            open_and_write(Request, fun cowboy_req:part_body/1, Req);
        _ ->
            handle_multipart(Request, ReqContentType, Req)
    end.

send_file(File, Req) ->
    {File2, Data} = case francus:read(File) of
                        eof -> {File, <<>>};
                        X -> X
                    end,
    #{<<"content-type">> := ContentType} = francus:metadata(File2),
    francus:close(File2),
    _ = mongoose_metrics:update({wocky_app:server(),
                                 tros_francus_bytes_sent},
                                byte_size(Data)),
    cowboy_req:reply(200, [{<<"content-type">>, ContentType}], Data, Req).

open_and_write(Request = #tros_request{user = User, file = FileID,
                                       access = Access,
                                       metadata = Metadata},
               ChunkFun, Req) ->
    {ok, F} = francus:open_write(wocky_app:server(),
                                 FileID, User, Access, Metadata),
    write_data(F, Request, ChunkFun, Req).

write_data(F, Request = #tros_request{size = SizeRemaining}, ChunkFun, Req) ->
    {Result, Data, NewSizeRemaining, Req2} =
    get_data(SizeRemaining, ChunkFun, Req),
    Request2 = Request#tros_request{size = NewSizeRemaining},
    _ = mongoose_metrics:update({wocky_app:server(),
                                 tros_francus_bytes_received},
                                byte_size(Data)),
    case Result of
        more ->
            F2 = francus:write(F, Data),
            write_data(F2, Request2, ChunkFun, Req2);
        ok ->
            F2 = francus:write(F, Data),
            finish_write(F2, Request2, Req2);
        excess_data ->
            finish_write(F, Request2, Req2)
    end.

get_data(SizeRemaining, ChunkFun, Req) ->
    {Result, Data, Req2} = ChunkFun(Req),
    NewSizeRemaining = SizeRemaining - byte_size(Data),
    case NewSizeRemaining of
        X when X >= 0 -> {Result, Data, NewSizeRemaining, Req2};
        _ ->             {excess_data, <<>>, NewSizeRemaining, Req2}
    end.

% Exactly the right number of bytes received and written. All is well.
finish_write(F, #tros_request{size = 0}, Req) ->
    ok = francus:close(F),
    cowboy_req:reply(200, Req);

% Too many or too few bytes received - close and delete the file and send an
% error code.
finish_write(F, #tros_request{file = File,
                              size = BytesRemaining}, Req) ->
    ok = francus:close(F),
    francus:delete(wocky_app:server(), File),
    case BytesRemaining of
        X when X > 0 ->
            {error, {400, "File was truncated", Req}};
        X when X < 0 ->
            {error, {413, "", Req}}
    end.

-spec terminate({normal, shutdown} | {error, atom()}, cowboy_req:req(), any())
                -> ok.
terminate(_Reason, _Req, _State) -> ok.
