%%% @copyright 2016+ Hippware, Inc.
%%% @doc Cowboy HTTP server listener for TROS-Francus interface
-module(tros_francus_http).

-include("mod_tros_francus.hrl").

-compile({parse_transform, do}).

-export([init/3,
         handle/2,
         terminate/3]).

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
        {Method, Req2} <- check_method(Req),
        {AuthReq, Req3} <- check_auth(Method, Req2),
        do_op(Method, AuthReq, Req3)]).

check_method(Req) ->
    case cowboy_req:method(Req) of
        {<<"GET">>, Req2} -> {ok, {get, Req2}};
        {<<"POST">>, Req2} -> {ok, {post, Req2}};
        {_, Req2} -> {error, {405, "Unsupported method.", Req2}}
    end.

check_auth(Method, Req) ->
    {Path, Req2} = cowboy_req:path(Req),
    {User, File} = user_file(Path),
    {Auth, Req3} = cowboy_req:header(<<"authorization">>, Req2, <<>>),
    case tros_req_tracker:check(User, File, Auth, Method) of
        false -> {error, {401, "Unauthorised request", Req3}};
        AuthReq -> {ok, {AuthReq, Req3}}
    end.

plain_header() -> [{<<"content-type">>, <<"text/plain">>}].

user_file(<<$/, Path/binary>>) ->
    case binary:split(Path, <<$/>>, [global]) of
        [<<"users">>, User, <<"files">>, File] -> {User, File};
        _ -> {<<>>, <<>>}
    end.

do_op(get, #tros_request{file = FileID}, Req) ->
    case francus:open_read(wocky_app:server(), FileID) of
        %% TODO: Chunked writes for larger files?
        {ok, F} ->
            send_file(F, Req);
        {error, not_found} ->
            {error, {404, "Not found", Req}}
    end;

do_op(post, Request = #tros_request{user = User,
                                    file = FileID,
                                    metadata = Metadata =
                                    #{<<"content-type">> := ContentType}
                                   }, Req) ->
    {ReqContentType, Req2} = cowboy_req:header(<<"content-type">>, Req, <<>>),
    case ReqContentType of
        ContentType ->
            {ok, F} = francus:open_write(wocky_app:server(),
                                         FileID, User, Metadata),
            write_data(F, Request, Req2);
        _ ->
            {error, {415, "Mismatched media types between IQ and POST", Req2}}
    end.

send_file(File, Req) ->
    {File2, Data} = case francus:read(File) of
                        eof -> {File, <<>>};
                        X -> X
                    end,
    #{<<"content-type">> := ContentType} = francus:metadata(File2),
    francus:close(File2),
    cowboy_req:reply(200, [{<<"content-type">>, ContentType}], Data, Req).


write_data(F, Request = #tros_request{size = SizeRemaining}, Req) ->
    {Result, Data, NewSizeRemaining, Req2} = get_data(SizeRemaining, Req),
    Request2 = Request#tros_request{size = NewSizeRemaining},
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
