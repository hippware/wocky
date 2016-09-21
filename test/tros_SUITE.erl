%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite for TROS
-module(tros_SUITE).
-compile(export_all).
-compile({parse_transform, fun_chain}).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("common_test/include/ct.hrl").

-include("wocky_db_seed.hrl").
-include("wocky.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     file_updown_story,
     multipart_story,
     avatar_updown_story,
     message_media_updown_story,
     file_down_bob_story,
     file_down_carol_story,
     file_up_too_big_story,
     file_up_too_small_story,
     request_too_big_story,
     wrong_purpose_story,
     wrong_type_story
    ].

suite() ->
    escalus:suite().


%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),
    ejabberd_config:del_local_option(tros_public_port),
    ejabberd_config:add_local_option(tros_public_port, 1025),
    wocky_db:clear_user_tables(?LOCAL_CONTEXT),
    wocky_db:clear_tables(?LOCAL_CONTEXT, [media, media_data]),
    wocky_db_seed:seed_tables(?LOCAL_CONTEXT, [media, media_data]),
    Users = escalus:get_users([alice, bob, carol, karen]),
    fun_chain:first(Config,
        escalus:init_per_suite(),
        escalus:create_users(Users)
    ).

end_per_suite(Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob, carol, karen])),
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).


%%--------------------------------------------------------------------
%% mod_wocky_tros tests
%%--------------------------------------------------------------------

file_updown_story(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        ImageData = load_test_file(Config),
        FileSize = byte_size(ImageData),

        %%% Upload
        {QueryStanza, ResultStanza} =
        common_upload_request(FileSize, Alice, avatar_purpose()),
        escalus:assert(is_iq_result, [QueryStanza], ResultStanza),
        FileID = do_upload(ResultStanza, ImageData, 200, false),

        %% Download
        download_success(Alice, FileID, ImageData)
    end).

multipart_story(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        ImageData = load_test_file(Config),
        FileSize = byte_size(ImageData),

        %%% Upload
        {QueryStanza, ResultStanza} =
        common_upload_request(FileSize, Alice, avatar_purpose()),
        escalus:assert(is_iq_result, [QueryStanza], ResultStanza),
        FileID = do_upload(ResultStanza, ImageData, 200, true),

        %% Download
        download_success(Alice, FileID, ImageData)
    end).

avatar_updown_story(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        ImageData = crypto:strong_rand_bytes(5000),
        FileSize = byte_size(ImageData),

        %%% Upload
        {QueryStanza, ResultStanza} =
        common_upload_request(FileSize, Alice, avatar_purpose()),
        escalus:assert(is_iq_result, [QueryStanza], ResultStanza),
        FileID = do_upload(ResultStanza, ImageData, 200, false),

        %% Download
        download_success(Bob, FileID, ImageData)
    end).

message_media_updown_story(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}, {carol, 1}, {karen, 1}],
                  fun(Alice, Bob, Carol, Karen) ->
        ImageData = crypto:strong_rand_bytes(5000),
        FileSize = byte_size(ImageData),

        %%% Upload
        {QueryStanza, ResultStanza} =
        common_upload_request(FileSize, Alice, message_purpose(),
                              access_list([Bob, Carol])),
        escalus:assert(is_iq_result, [QueryStanza], ResultStanza),
        FileID = do_upload(ResultStanza, ImageData, 200, false),

        %% Download - Successes
        lists:foreach(fun(C) ->
                              download_success(C, FileID, ImageData)
                      end,
                      [Alice, Bob, Carol]),

        download_failure(Karen, FileID)
    end).

file_down_bob_story(Config) ->
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
        %% Download avatar
        download_success(Bob, ?AVATAR_FILE, ?AVATAR_DATA),

        %% Download media file
        download_success(Bob, ?MEDIA_FILE, ?MEDIA_DATA)
    end).

file_down_carol_story(Config) ->
    escalus:story(Config, [{carol, 1}], fun(Carol) ->
        %% Fail to download media file since we are neither the owner
        %% nor the participant
        download_failure(Carol, ?MEDIA_DATA)
    end).


file_up_too_big_story(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        ImageData = load_test_file(Config),
        FileSize = byte_size(ImageData),

        {QueryStanza, ResultStanza} =
        common_upload_request(FileSize div 2, Alice, avatar_purpose()),
        escalus:assert(is_iq_result, [QueryStanza], ResultStanza),
        do_upload(ResultStanza, ImageData, 413, false)
    end).

file_up_too_small_story(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        ImageData = load_test_file(Config),
        FileSize = byte_size(ImageData),

        {QueryStanza, ResultStanza} =
        common_upload_request(FileSize * 2, Alice, avatar_purpose()),

        escalus:assert(is_iq_result, [QueryStanza], ResultStanza),
        do_upload(ResultStanza, ImageData, 400, false)
    end).

request_too_big_story(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        {_, ResultStanza} = common_upload_request((1024*1024*10)+1,
                                                  Alice, avatar_purpose()),
        escalus:assert(is_iq_error, ResultStanza)
    end).

wrong_purpose_story(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        {_, ResultStanza} =
        common_upload_request(1204, Alice, <<"invalidpurpose:abc">>),
        escalus:assert(is_iq_error, ResultStanza)
    end).

wrong_type_story(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        {QueryStanza, ResultStanza} = common_upload_request(1204,
                                                            Alice,
                                                            avatar_purpose()),
        escalus:assert(is_iq_result, [QueryStanza], ResultStanza),
        do_upload(ResultStanza, <<"datadata">>, 415, "image/jpeg", false)
    end).


%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

common_upload_request(Size, Client, Purpose) ->
    common_upload_request(Size, Client, Purpose, <<"all">>).

common_upload_request(Size, Client, Purpose, Access) ->
    QueryStanza = upload_stanza(<<"image.png">>,
                                Size, <<"image/png">>,
                                Purpose, Access),
    FinalQueryStanza = add_to_from(QueryStanza, Client),
    ResultStanza = escalus:send_and_wait(Client, FinalQueryStanza),
    {QueryStanza, ResultStanza}.

load_test_file(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    ImageFile = filename:join(DataDir, "test_image.png"),
    {ok, ImageData} = file:read_file(ImageFile),
    ImageData.

add_to_from(Stanza, Client) ->
    escalus_stanza:to(
      escalus_stanza:from(Stanza, Client),
      escalus_client:server(Client)).

do_upload(ResultStanza, ImageData, ExpectedCode, Multipart) ->
    do_upload(ResultStanza, ImageData, ExpectedCode, undefined, Multipart).

-define(MP_BOUNDARY, "------").

do_upload(ResultStanza, ImageData, ExpectedCode, ContentType, Multipart) ->
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
    {OuterContentType, Body} =
    case Multipart of
        false ->
            {ReqContentType, ImageData};
        true ->
            {"multipart/form-data; boundary=" ++ ?MP_BOUNDARY,
            make_multipart_body(ReqContentType, ImageData)}
    end,
    {ok, Result} = httpc:request(list_to_atom(
                                   string:to_lower(
                                     binary_to_list(Method))),
                                 {binary_to_list(URL),
                                  FinalHeaders,
                                  OuterContentType,
                                  Body},
                                 [], []),
    {Response, _RespHeaders, _RespContent} = Result,
    {_, ExpectedCode, _} = Response,
    FileID.

make_multipart_body(ContentType, ImageData) ->
    Prefix = cow_multipart:part(?MP_BOUNDARY, [{"content-type", ContentType}]),
    iolist_to_binary([Prefix, ImageData, cow_multipart:close(?MP_BOUNDARY)]).

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
    true = lists:member({"content-length",
                         integer_to_list(byte_size(RespBin))},
                        RespHeaders),
    true = lists:member({"content-type", "image/png"}, RespHeaders),
    RespBin.

request_wrapper(Type, Name, DataFields) ->
    test_helper:iq_with_type(Type, ?NS_TROS,
                             #xmlel{name = Name, children = DataFields}).

upload_stanza(FileName, Size, Type, Purpose, Access) ->
    FieldData = [{<<"filename">>, FileName},
                 {<<"size">>, integer_to_list(Size)},
                 {<<"mime-type">>, Type},
                 {<<"purpose">>, Purpose},
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

avatar_purpose() ->
    <<"avatar">>.
message_purpose() ->
    <<"message_media">>.

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
    DLResultStanza = escalus:send_and_wait(Client, FinalDLStanza),
    escalus:assert(is_iq_result, [DLQueryStanza], DLResultStanza),
    Data = do_download(DLResultStanza).

download_failure(Client, FileID) ->
    DLQueryStanza = download_stanza(FileID),
    FinalDLStanza = add_to_from(DLQueryStanza, Client),
    DLResultStanza = escalus:send_and_wait(Client, FinalDLStanza),
    escalus:assert(is_iq_error, DLResultStanza).
