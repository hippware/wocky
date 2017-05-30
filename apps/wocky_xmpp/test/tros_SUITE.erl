%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite for TROS
-module(tros_SUITE).

-compile(export_all).
-compile({parse_transform, fun_chain}).
-compile({parse_transform, cut}).

-include_lib("common_test/include/ct.hrl").
-include("test_helper.hrl").

-define(S3_TIMEOUT, 5000).


%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     %% Uncomment this to enable tests against real-world S3:
%     {group, s3}
    ].

groups() ->
    [
     {s3,
      [
       file_updown_story,
       message_media_updown_story,
       update_metadata,
       file_up_too_big_story,
       request_too_big_story
      ]
     }
    ].

suite() ->
    escalus:suite().


%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),
    fun_chain:first(Config,
        escalus:init_per_suite(),
        test_helper:setup_users([alice, bob, carol, karen])
    ).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).


%%--------------------------------------------------------------------
%% mod_wocky_tros tests
%%--------------------------------------------------------------------

file_updown_story(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        ImageData = load_test_in_file(Config),
        FileSize = byte_size(ImageData),

        %%% Upload
        {QueryStanza, ResultStanza} =
        common_upload_request(FileSize, Alice),
        escalus:assert(is_iq_result, [QueryStanza], ResultStanza),
        FileID = do_upload(ResultStanza, ImageData, 200),

        %% Download
        download_success(Alice, FileID, load_test_out_file(Config)),
        download_success(Alice, <<FileID/binary, "-thumbnail">>,
                         load_test_thumb_file(Config)),
        download_success(Alice, <<FileID/binary, "-original">>, ImageData),
        download_success(Bob, FileID, load_test_out_file(Config))
    end).

message_media_updown_story(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}, {karen, 1}],
                  fun(Alice, Bob, Carol, Karen) ->
        ImageData = load_test_in_file(Config),
        FileSize = byte_size(ImageData),

        %%% Upload
        {QueryStanza, ResultStanza} =
        common_upload_request(FileSize, Alice, access_list([Bob, Carol])),
        escalus:assert(is_iq_result, [QueryStanza], ResultStanza),
        FileID = do_upload(ResultStanza, ImageData, 200),

        %% Download - Successes
        lists:foreach(fun(C) ->
                              download_success(C, FileID,
                                               load_test_out_file(Config))
                      end,
                      [Alice, Bob, Carol]),

        download_failure(Karen, FileID)
    end).

file_up_too_big_story(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        ImageData = load_test_in_file(Config),
        FileSize = byte_size(ImageData),

        {QueryStanza, ResultStanza} =
        common_upload_request(FileSize div 2, Alice),
        escalus:assert(is_iq_result, [QueryStanza], ResultStanza),
        do_upload(ResultStanza, ImageData, 403)
    end).

request_too_big_story(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        {_, ResultStanza} = common_upload_request((1024*1024*10)+1, Alice),
        escalus:assert(is_iq_error, ResultStanza)
    end).

update_metadata(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        ImageData = load_test_in_file(Config),
        FileSize = byte_size(ImageData),

        %%% Upload
        {QueryStanza, ResultStanza} =
        common_upload_request(FileSize, Alice, <<"">>),
        escalus:assert(is_iq_result, [QueryStanza], ResultStanza),
        FileID = do_upload(ResultStanza, ImageData, 200),

        OutData = load_test_out_file(Config),
        download_success(Alice, FileID, OutData),
        %%% Bob's download should fail because he lacks access
        download_failure(Bob, FileID),

        %%% Now let's modify the metadata
        ?tros:update_access(FileID, <<"all">>),

        %%% Now both Alice and Bob should have access
        download_success(Alice, FileID, OutData),
        download_success(Bob, FileID, OutData)
    end).


%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

common_upload_request(Size, Client) ->
    common_upload_request(Size, Client, <<"all">>).

common_upload_request(Size, Client, Access) ->
    QueryStanza = upload_stanza(<<"image.png">>,
                                Size, <<"image/png">>,
                                Access),
    FinalQueryStanza = add_to_from(QueryStanza, Client),
    ResultStanza = escalus:send_and_wait(Client, FinalQueryStanza),
    {QueryStanza, ResultStanza}.

load_test_in_file(Config) ->
    load_file(Config, "test_image.png").

load_test_out_file(Config) ->
    load_file(Config, "test_image_out.png").

load_test_thumb_file(Config) ->
    load_file(Config, "test_image_thumb.png").

load_file(Config, FileName) ->
    DataDir = proplists:get_value(data_dir, Config),
    ImageFile = filename:join(DataDir, FileName),
    {ok, ImageData} = file:read_file(ImageFile),
    ImageData.

add_to_from(Stanza, Client) ->
    escalus_stanza:to(
      escalus_stanza:from(Stanza, Client),
      escalus_client:server(Client)).

do_upload(ResultStanza, ImageData, ExpectedCode) ->
    do_upload(ResultStanza, ImageData, ExpectedCode, undefined).

do_upload(ResultStanza, ImageData, ExpectedCode, ContentType) ->
    UploadEl = get_element(ResultStanza, <<"upload">>),
    URL = get_cdata(UploadEl, <<"url">>),
    Method = get_cdata(UploadEl, <<"method">>),
    HeadersEl = get_element(UploadEl, <<"headers">>),
    FileID = get_cdata(UploadEl, <<"id">>),
    Headers = get_headers(HeadersEl),
    ReqContentType =
    case ContentType of
        undefined -> proplists:get_value("content-type", Headers);
        _ -> ContentType
    end,
    FinalHeaders = Headers -- [ContentType],

    {ok, Result} = httpc:request(list_to_atom(
                                   string:to_lower(
                                     binary_to_list(Method))),
                                 {binary_to_list(URL),
                                  FinalHeaders,
                                  ReqContentType,
                                  ImageData},
                                 [], []),
    {Response, _RespHeaders, _RespContent} = Result,
    {_, ExpectedCode, _} = Response,
    FileID.

do_download(ResultStanza) ->
    DownloadEl = get_element(ResultStanza, <<"download">>),
    URL = get_cdata(DownloadEl, <<"url">>),
    HeadersEl = get_element(DownloadEl, <<"headers">>),
    Headers = get_headers(HeadersEl),

    {ok, Result} = httpc:request(get,
                                 {binary_to_list(URL), Headers},
                                 [], []),
    {Response, RespHeaders, RespContent} = Result,
    {_, 200, "OK"} = Response,
    RespBin = list_to_binary(RespContent),
    NormHeaders = normalise_headers(RespHeaders),
    true = lists:member({"content-length",
                         integer_to_list(byte_size(RespBin))},
                        NormHeaders),
    true = lists:member({"content-type", "image/png"}, NormHeaders),
    RespBin.

normalise_headers(Headers) ->
    lists:map(fun({K, V}) -> {string:to_lower(K), V} end, Headers).

request_wrapper(Type, Name, DataFields) ->
    test_helper:iq_with_type(Type, ?NS_TROS,
                             #xmlel{name = Name, children = DataFields}).

upload_stanza(FileName, Size, Type, Access) ->
    FieldData = [{<<"filename">>, FileName},
                 {<<"size">>, integer_to_list(Size)},
                 {<<"mime-type">>, Type},
                 {<<"access">>, Access}
                ],
    UploadFields = [#xmlel{name = N, children = [#xmlcdata{content = V}]}
                    || {N, V} <- FieldData],
    request_wrapper(<<"set">>, <<"upload-request">>, UploadFields).

download_stanza(FileID) ->
    Field = #xmlel{name = <<"id">>, children = [#xmlcdata{content = FileID}]},
    request_wrapper(<<"get">>, <<"download-request">>, [Field]).

get_headers(HeadersEl) ->
    [get_header(HeaderEl)
     || HeaderEl <- exml_query:paths(HeadersEl, [{element, <<"header">>}])].

get_header(HeaderEl) ->
    list_to_tuple(
      [binary_to_list(exml_query:path(HeaderEl, [{attr, Attr}]))
       || Attr <- [<<"name">>, <<"value">>]]).

get_cdata(Element, Name) ->
    exml_query:path(Element, [{element, Name}, cdata]).

get_element(ParentElement, Name) ->
    exml_query:path(ParentElement, [{element, Name}]).

access_list(Clients) ->
    JIDs = [escalus_client:short_jid(C) || C <- Clients],
    lists:foldl(fun(JID, Acc) ->
                        <<Acc/binary, ",", (user_access(JID))/binary>>
                end, user_access(hd(JIDs)), tl(JIDs)).

user_access(JID) ->
    <<"user:", JID/binary>>.

download_success(Client, FileID, Data) ->
    DLQueryStanza = download_stanza(FileID),
    FinalDLStanza = add_to_from(DLQueryStanza, Client),
    escalus:send(Client, FinalDLStanza),
    DLResultStanza = escalus:wait_for_stanza(Client, ?S3_TIMEOUT),
    escalus:assert(is_iq_result, [DLQueryStanza], DLResultStanza),
    Data = do_download(DLResultStanza).

download_failure(Client, FileID) ->
    DLQueryStanza = download_stanza(FileID),
    FinalDLStanza = add_to_from(DLQueryStanza, Client),
    DLResultStanza = escalus:send_and_wait(Client, FinalDLStanza),
    escalus:assert(is_iq_error, DLResultStanza).
