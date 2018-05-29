%%% @copyright 2016+ Hippware, Inc.
%%% @doc Implementation module for TROS file transfer system
%%% (https://github.com/hippware/tr-wiki/wiki/HXEP%3A-Files-over-http).
%%%
-module(mod_wocky_tros).

-compile({parse_transform, do}).

-include("wocky.hrl").

-behaviour(gen_mod).

% gen_mod exports
-export([
   start/2,
   stop/1,
   handle_iq/3,
   waiter_event/1
        ]).

% Other XMPP TROS functions
-export([
   get_download_urls/2
        ]).

-ifdef(TEST).
-export([make_file_id/0, wait_ready/1]).
-endif.

-record(request, {
          from_jid :: ejabberd:jid(),
          to_jid :: ejabberd:jid(),
          iq :: iq()
         }).

-define(DEFAULT_MAX_UPLOAD_SIZE, (1024*1024 * 10)). % 10MB

-ifdef(TEST).
-define(PROCESSING_TIMEOUT, timer:seconds(2)).
-else.
-define(PROCESSING_TIMEOUT, timer:minutes(5)).
-endif.

start(Host, Opts) ->
    ?wocky_xmpp_tros_metadata_callbacks:register(),
    wocky_util:set_config_from_opt(max_upload_size, tros_max_upload_size,
                                   ?DEFAULT_MAX_UPLOAD_SIZE, Opts),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_TROS,
                                  ?MODULE, handle_iq, parallel),
    setup_metrics(),
    ok.

stop(Host) ->
    _ = gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_TROS),
    ok.

-spec handle_iq(From :: ejabberd:jid(),
                To :: ejabberd:jid(),
                IQ :: iq()) -> ignore | iq().
handle_iq(FromJID, ToJID, IQ = #iq{type = Type, sub_el = ReqEl}) ->
    Req = #request{from_jid = FromJID, to_jid = ToJID, iq = IQ},
    case handle_request(Req, Type, ReqEl) of
        {error, E} ->
            _ = inc_counter(wocky_tros_failed_requests_total),
            error_response(IQ, E);
        {ok, R} -> R
    end.

handle_request(Req, get, ReqEl = #xmlel{name = <<"download-request">>}) ->
    handle_download_request(Req, ReqEl);
handle_request(Req, set, ReqEl = #xmlel{name = <<"upload-request">>}) ->
    handle_upload_request(Req, ReqEl);
handle_request(_, _, _) ->
    {ok, ignore}.

handle_download_request(Req = #request{from_jid = FromJID}, DR) ->
    do([error_m ||
        Fields <- extract_fields(DR, [<<"id">>], [], #{}),
        FileID <- check_file_id(Fields),
        BaseID <- {ok, ?tros:get_base_id(FileID)},
        #{user_id := OwnerID, access := Access}
            <- expand_err(?tros:get_metadata(BaseID)),
        check_download_permissions(FromJID, OwnerID, Access),
        wait_ready(BaseID),
        {ok, inc_counter(wocky_tros_download_requests_total)},
        download_response(Req, OwnerID, FileID)
       ]).

handle_upload_request(Req, UR) ->
    RequiredFields = [<<"filename">>, <<"size">>, <<"mime-type">>],
    OptionalFields = [<<"access">>, <<"width">>, <<"height">>],
    Defaults = #{<<"access">> => <<>>},
    do([error_m ||
        Fields <- extract_fields(UR, RequiredFields, OptionalFields, Defaults),
        Size <- check_upload_size(Fields),
        check_access(Fields),
        {ok, inc_counter(wocky_tros_upload_requests_total)},
        upload_response(Req, Fields, Size)
       ]).

% Gets full and thumbnail URLs for the specified image
-spec get_download_urls(binary(), ejabberd:jid()) -> {binary(), binary()}.
get_download_urls(URL, FromJID) ->
    Result =
    do([error_m ||
        {Server, FileID} <- ?tros:parse_url(URL),
        #{user_id := OwnerID, access := Access} = Metadata
            <- expand_err(?tros:get_metadata(FileID)),
        check_download_permissions(FromJID, OwnerID, Access),
        {ok, {Server, Metadata}}
       ]),
    case Result of
        {ok, {Server, Metadata}} ->
            list_to_tuple(
              ?tros:get_download_urls(Server, Metadata, [full, thumbnail]));
        _ ->
            {<<>>, <<>>}
    end.

wait_ready(FileID) ->
    SkipFun = fun() -> ?tros:'ready?'(FileID) end,
    Event = waiter_event(FileID),
    case ?wocky_waiter:wait(Event, ?PROCESSING_TIMEOUT, SkipFun) of
        ok ->
            ok;
        timeout ->
            {error, ?ERRT_INTERNAL_SERVER_ERROR(
                        ?MYLANG, <<"Timeout waiting for file to be ready">>)}
    end.

extract_fields(Req, RequiredFields, OptionalFields, Defaults) ->
    Fields = lists:foldl(fun(F, Acc) -> add_field(Req, F, Acc) end,
                         Defaults,
                         RequiredFields ++ OptionalFields),
    check_fields(Fields, RequiredFields).

add_field(UR, Field, Acc) ->
    case exml_query:path(UR, [{element, Field}, cdata]) of
        undefined ->
            Acc;
        Value when is_binary(Value) ->
            Acc#{Field => Value}
    end.

check_fields(Fields, []) -> {ok, Fields};
check_fields(Fields, [H|T]) ->
    case maps:is_key(H, Fields) of
        false -> {error, missing_field_error(H)};
        true -> check_fields(Fields, T)
    end.

missing_field_error(FieldName) ->
    Text = iolist_to_binary(["Required field missing: ", FieldName]),
    {error, ?ERRT_BAD_REQUEST(?MYLANG, Text)}.

upload_validation_error(ErrorStr) ->
    Text = iolist_to_binary(["Upload request denied: ", ErrorStr]),
    validation_error(Text).

download_validation_error(ErrorStr) ->
    Text = iolist_to_binary(["Download request denied: ", ErrorStr]),
    validation_error(Text).

validation_error(Text) ->
    {error, ?ERRT_NOT_ACCEPTABLE(?MYLANG, Text)}.

check_file_id(#{<<"id">> := <<"tros:", JID/binary>>}) ->
    case jid:from_binary(JID) of
        #jid{lresource = <<"file/", LResource/binary>>} ->
            check_file_id(LResource);
        _ ->
            download_validation_error("Invalid file URL")
    end;

check_file_id(#{<<"id">> := ID}) ->
    check_file_id(ID);

check_file_id(ID) when is_binary(ID) ->
    {ok, ID}.

check_upload_size(#{<<"size">> := SizeBin}) ->
    Size = binary_to_integer_def(SizeBin, 0),
    MaxSize = ejabberd_config:get_local_option(tros_max_upload_size),
    case Size =< MaxSize andalso Size > 0 of
        true -> {ok, Size};
        false -> upload_validation_error(["Invalid size: ", SizeBin])
    end.

check_access(#{<<"access">> := Access}) ->
    case tros_permissions:is_valid(Access) of
        true -> ok;
        false -> {error, ?ERRT_BAD_REQUEST(
                            ?MYLANG, <<"Invalid access string">>)}
    end.

check_download_permissions(FromJID, OwnerID, Access) ->
    case tros_permissions:can_download(FromJID, OwnerID, Access) of
        true ->
            ok;
        {false, Reason} ->
            {error, ?ERRT_FORBIDDEN(?MYLANG,
                                    iolist_to_binary(
                                      ["No permission to download this file: ",
                                       atom_to_list(Reason)]))}
    end.

upload_response(Req = #request{from_jid = FromJID},
                #{<<"mime-type">> := MimeType,
                  <<"access">> := Access,
                  <<"filename">> := Filename},
                Size) ->
    Metadata = #{<<"content-type">> => MimeType, <<"name">> => Filename},
    FileID = make_file_id(),
    case ?tros:make_upload_response(FromJID, FileID, Size, Access, Metadata) of
        {ok, {Headers, RespFields}} ->
            FullFields = common_fields(FromJID, FileID) ++ RespFields,
            response(Req, Headers, FullFields, <<"upload">>);

        {error, Errors} ->
            upload_validation_error(
                ?wocky_errors:render_errors(
                    ?wocky_errors:to_map(Errors)))
    end.

download_response(Req = #request{from_jid = FromJID}, _OwnerID, FileID) ->
    {ok, {Headers, RespFields}} =
        ?tros:make_download_response(FromJID#jid.lserver, FileID),

    response(Req, Headers, RespFields, <<"download">>).

response(#request{iq = IQ}, Headers, RespFields, RespType) ->
    HeaderElement = #xmlel{name = <<"headers">>,
                           children =
                           [to_header_element(H) || H <- Headers]},

    ActionElement = #xmlel{name = RespType,
                           children =
                           [HeaderElement |
                            [to_xmlel(F) || F <- RespFields]]},

    {ok, IQ#iq{type = result, sub_el = ActionElement}}.

error_response(IQ = #iq{sub_el = SubEl}, Error) ->
    IQ#iq{type = error, sub_el = [SubEl, Error]}.

common_fields(#jid{lserver = Server}, FileID) ->
    [{<<"id">>, FileID},
     {<<"jid">>, jid:to_binary(?tros:make_jid(Server, FileID))}].

to_header_element({Name, Value}) ->
    #xmlel{name = <<"header">>,
           attrs = [{<<"name">>, Name}, {<<"value">>, Value}]}.

to_xmlel({Name, Content}) ->
    #xmlel{name = Name, children = [#xmlcdata{content = Content}]}.

make_file_id() ->
    ?wocky_id:new().

binary_to_integer_def(Binary, Default) ->
    try
        binary_to_integer(Binary)
    catch
        error:badarg -> Default
    end.

expand_err({error, not_found}) ->
    Text = <<"File not found">>,
    {error, ?ERRT_ITEM_NOT_FOUND(?MYLANG, Text)};
expand_err(NonError) -> NonError.

setup_metrics() ->
    prometheus_counter:new([{name, wocky_tros_download_requests_total},
                            {help, "File download request count"},
                            {labels, [server]}]),
    prometheus_counter:new([{name, wocky_tros_upload_requests_total},
                            {help, "File upload request count"},
                            {labels, [server]}]),
    prometheus_counter:new([{name, wocky_tros_failed_requests_total},
                            {help, "Failed file request count"},
                            {labels, [server]}]),
    ok.

inc_counter(Name) ->
    _ = prometheus_counter:inc(Name, [wocky_xmpp_app:server()]),
    ok.

waiter_event(ID) -> <<"tros_waiter_", ID/binary>>.
