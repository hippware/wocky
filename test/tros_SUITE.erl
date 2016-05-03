%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite for TROS
-module(tros_SUITE).
-compile(export_all).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("common_test/include/ct.hrl").

-include("wocky_db_seed.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, tros}
    ].

groups() ->
    [
     {tros, [sequence], [file_updown_story,
                         multipart_story,
                         file_down_bob_story,
                         file_down_carol_story,
                         file_up_too_big_story,
                         file_up_too_small_story,
                         request_too_big_story,
                         wrong_purpose_story,
                         wrong_type_story
                        ]}
    ].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    test_helper:start_ejabberd(),
    ejabberd_config:del_local_option(tros_public_port),
    ejabberd_config:add_local_option(tros_public_port, 1025),
    wocky_db_seed:clear_user_tables(?LOCAL_CONTEXT),
    wocky_db_seed:clear_tables(?LOCAL_CONTEXT, [media, media_data]),
    wocky_db_seed:seed_tables(?LOCAL_CONTEXT, [media, media_data]),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config),
    test_helper:stop_ejabberd(),
    ok.

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config),
    Config2 = escalus:make_everyone_friends(Config),
    escalus_ejabberd:wait_for_session_count(Config2, 0),
    Config2.

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% mod_tros tests
%%--------------------------------------------------------------------

file_updown_story(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        ImageData = load_test_file(Config),
        FileSize = byte_size(ImageData),


        %%% Upload
        {QueryStanza, ResultStanza} =
        common_upload_request(FileSize, Config, Alice, avatar_purpose()),
        escalus:assert(is_iq_result, [QueryStanza], ResultStanza),
        FileID = do_upload(ResultStanza, ImageData, 200, false),


        %% Download
        DLQueryStanza = download_stanza(<<"456">>, FileID),
        FinalDLStanza = add_to_from(Config, DLQueryStanza, alice),
        DLResultStanza = escalus:send_and_wait(Alice, FinalDLStanza),
        escalus:assert(is_iq_result, [DLQueryStanza], DLResultStanza),
        ImageData = do_download(DLResultStanza)
    end).

multipart_story(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        ImageData = load_test_file(Config),
        FileSize = byte_size(ImageData),

        %%% Upload
        {QueryStanza, ResultStanza} =
        common_upload_request(FileSize, Config, Alice, avatar_purpose()),
        escalus:assert(is_iq_result, [QueryStanza], ResultStanza),
        FileID = do_upload(ResultStanza, ImageData, 200, true),

        %% Download
        DLQueryStanza = download_stanza(<<"456">>, FileID),
        FinalDLStanza = add_to_from(Config, DLQueryStanza, alice),
        DLResultStanza = escalus:send_and_wait(Alice, FinalDLStanza),
        escalus:assert(is_iq_result, [DLQueryStanza], DLResultStanza),
        ImageData = do_download(DLResultStanza)
    end).



file_down_bob_story(Config) ->
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
        %% Download avatar
        DLQueryStanza = download_stanza(<<"800">>, ?AVATAR_FILE),
        FinalDLStanza = add_to_from(Config, DLQueryStanza, bob),
        DLResultStanza = escalus:send_and_wait(Bob, FinalDLStanza),
        escalus:assert(is_iq_result, [DLQueryStanza], DLResultStanza),
        ?AVATAR_DATA = do_download(DLResultStanza),

        %% Download media file
        DLQueryStanza2 = download_stanza(<<"801">>, ?MEDIA_FILE),
        FinalDLStanza2 = add_to_from(Config, DLQueryStanza2, bob),
        DLResultStanza2 = escalus:send_and_wait(Bob, FinalDLStanza2),
        escalus:assert(is_iq_result, [DLQueryStanza2], DLResultStanza2),
        ?MEDIA_DATA = do_download(DLResultStanza2)
    end).

file_down_carol_story(Config) ->
    escalus:story(Config, [{carol, 1}], fun(Carol) ->
        %% Fail to download media file since we are neither the owner
        %% nor the participant
        DLQueryStanza = download_stanza(<<"805">>, ?MEDIA_FILE),
        FinalDLStanza = add_to_from(Config, DLQueryStanza, carol),
        DLResultStanza = escalus:send_and_wait(Carol, FinalDLStanza),
        escalus:assert(is_iq_error, DLResultStanza)
    end).


file_up_too_big_story(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        ImageData = load_test_file(Config),
        FileSize = byte_size(ImageData),

        {QueryStanza, ResultStanza} =
        common_upload_request(FileSize div 2, Config, Alice, avatar_purpose()),
        escalus:assert(is_iq_result, [QueryStanza], ResultStanza),
        do_upload(ResultStanza, ImageData, 413, false)
    end).

file_up_too_small_story(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        ImageData = load_test_file(Config),
        FileSize = byte_size(ImageData),

        {QueryStanza, ResultStanza} =
        common_upload_request(FileSize * 2, Config, Alice, avatar_purpose()),

        escalus:assert(is_iq_result, [QueryStanza], ResultStanza),
        do_upload(ResultStanza, ImageData, 400, false)
    end).

request_too_big_story(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        {_, ResultStanza} = common_upload_request((1024*1024*10)+1,
                                             Config, Alice, avatar_purpose()),
        escalus:assert(is_iq_error, ResultStanza)
    end).

wrong_purpose_story(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        {_, ResultStanza} =
        common_upload_request(1204, Config, Alice, <<"invalidpurpose:abc">>),
        escalus:assert(is_iq_error, ResultStanza)
    end).

wrong_type_story(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        {QueryStanza, ResultStanza} = common_upload_request(1204,
                                                            Config, Alice,
                                                            avatar_purpose()),
        escalus:assert(is_iq_result, [QueryStanza], ResultStanza),
        do_upload(ResultStanza, <<"datadata">>, 415, "image/jpeg", false)
    end).


%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

common_upload_request(Size, Config, User, Purpose) ->
    QueryStanza = upload_stanza(<<"123">>, <<"image.png">>,
                                Size, <<"image/png">>,
                                Purpose),
    FinalQueryStanza = add_to_from(Config, QueryStanza, alice),
    ResultStanza = escalus:send_and_wait(User, FinalQueryStanza),
    {QueryStanza, ResultStanza}.

load_test_file(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    ImageFile = filename:join(DataDir, "test_image.png"),
    {ok, ImageData} = file:read_file(ImageFile),
    ImageData.

add_to_from(Config, Stanza, User) ->
    escalus_stanza:to(
      escalus_stanza:from(Stanza, User),
      escalus_users:get_server(Config, User)).

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

request_wrapper(ID, Type, Name, DataFields) ->
    #xmlel{name = <<"iq">>,
           attrs = [{<<"id">>, ID},
                    {<<"type">>, Type}],
           children = [#xmlel{name = Name,
                              attrs = [{<<"xmlns">>,
                                        <<"hippware.com/hxep/http-file">>}],
                              children = DataFields
                             }]}.

upload_stanza(ID, FileName, Size, Type, Purpose) ->
    FieldData = [{<<"filename">>, FileName},
                 {<<"size">>, integer_to_list(Size)},
                 {<<"mime-type">>, Type},
                 {<<"purpose">>, Purpose}],
    UploadFields = [#xmlel{name = N, children = [#xmlcdata{content = V}]}
                    || {N, V} <- FieldData],
    request_wrapper(ID, <<"set">>, <<"upload-request">>, UploadFields).

download_stanza(ID, FileID) ->
    Field = #xmlel{name = <<"id">>, children = [#xmlcdata{content = FileID}]},
    request_wrapper(ID, <<"get">>, <<"download-request">>, [Field]).

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
    <<"avatar:",
      (jid:to_binary(jid:make(?ALICE, ?LOCAL_CONTEXT, <<>>)))/binary>>.
