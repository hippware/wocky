%%% @copyright 2015+ Hippware, Inc.
%%% @doc Integration test suite for ejabberd
-module(ejabberd_integration_SUITE).
-compile(export_all).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, smoke},
     {group, last_activity},
     {group, offline},
     {group, hxep}
    ].

groups() ->
    [{smoke, [sequence], [messages_story]},
     {last_activity, [sequence], [activity_story,
                                  update_activity_story,
                                  server_uptime_story,
                                  unknown_user_acivity_story]},
     {offline, [sequence], [offline_message_story]},
     {hxep, [sequence], [file_updown_story,
                         file_up_too_big_story,
                         file_up_too_small_story,
                         request_too_big_story
                        ]}
    ].

suite() ->
    escalus:suite().


%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = test_helper:start_ejabberd(),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config),
    test_helper:stop_ejabberd().

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
%% Message tests
%%--------------------------------------------------------------------

messages_story(Config) ->
    %% Note that this story involves creating users and authenticating
    %% them via ejabberd_auth_wocky
    escalus:story(Config, [1, 1], fun(Alice, Bob) ->
        %% Alice sends a message to Bob
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),

        %% Bob gets the message
        escalus:assert(is_chat_message, [<<"OH, HAI!">>],
                       escalus:wait_for_stanza(Bob))
    end).


%%--------------------------------------------------------------------
%% mod_last tests
%%--------------------------------------------------------------------

activity_story(Config) ->
    % Last online story
    escalus:story(Config, [1, 1],
        fun(Alice, _Bob) ->
            %% Alice asks about Bob's last activity
            escalus_client:send(Alice, escalus_stanza:last_activity(bob)),

            %% server replies on Bob's behalf
            Stanza = escalus_client:wait_for_stanza(Alice),
            escalus:assert(is_last_result, Stanza),
            0 = get_last_activity(Stanza)
        end).

update_activity_story(Config) ->
    escalus:story(Config, [1],
        fun(Alice) ->
            %% Bob logs in
            {ok, Bob} = escalus_client:start_for(Config, bob, <<"bob">>),

            %% Bob logs out with a status
            Status = escalus_stanza:tags([{<<"status">>,
                                           <<"I am a banana!">>}]),
            Presence = escalus_stanza:presence(<<"unavailable">>, Status),
            escalus_client:send(Bob, Presence),
            escalus_client:stop(Bob),
            timer:sleep(1024), % more than a second

            %% Alice asks for Bob's last availability
            escalus_client:send(Alice, escalus_stanza:last_activity(bob)),

            %% Alice receives Bob's status and last online time > 0
            Stanza = escalus_client:wait_for_stanza(Alice),
            escalus:assert(is_last_result, Stanza),
            true = (1 =< get_last_activity(Stanza)),
            <<"I am a banana!">> = get_last_status(Stanza)
        end).

server_uptime_story(Config) ->
    escalus:story(Config, [1],
        fun(Alice) ->
            %% Alice asks for server's uptime
            Server = escalus_users:get_server(Config, alice),
            escalus_client:send(Alice, escalus_stanza:last_activity(Server)),

            %% Server replies with the uptime > 0
            Stanza = escalus_client:wait_for_stanza(Alice),
            escalus:assert(is_last_result, Stanza),
            true = (get_last_activity(Stanza) > 0)
        end).

unknown_user_acivity_story(Config) ->
    escalus:story(Config, [1],
        fun(Alice) ->
            escalus_client:send(Alice,
                                escalus_stanza:last_activity(<<"sven">>)),
            Stanza = escalus_client:wait_for_stanza(Alice),
            escalus:assert(is_error,
                           [<<"cancel">>, <<"service-unavailable">>], Stanza)
        end),
    ok.


get_last_activity(Stanza) ->
    S = exml_query:path(Stanza, [{element, <<"query">>},
                                 {attr, <<"seconds">>}]),
    list_to_integer(binary_to_list(S)).

get_last_status(Stanza) ->
    get_cdata(Stanza, <<"query">>).


%%--------------------------------------------------------------------
%% mod_offline tests
%%--------------------------------------------------------------------

offline_message_story(Config) ->
    %% Alice sends a message to Bob, who is offline
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        escalus:send(Alice, escalus_stanza:chat_to(bob, <<"Hi, Offline!">>))
    end),

    %% Bob logs in
    Bob = login_send_presence(Config, bob),

    %% He receives his initial presence and the message
    Stanzas = escalus:wait_for_stanzas(Bob, 2),
    escalus_new_assert:mix_match([is_presence,
                                  is_chat(<<"Hi, Offline!">>)],
                                 Stanzas),
    escalus_cleaner:clean(Config).

is_chat(Content) ->
    fun(Stanza) -> escalus_pred:is_chat_message(Content, Stanza) end.

login_send_presence(Config, User) ->
    Spec = escalus_users:get_userspec(Config, User),
    {ok, Client} = escalus_client:start(Config, Spec, <<"dummy">>),
    escalus:send(Client, escalus_stanza:presence(<<"available">>)),
    Client.


%%--------------------------------------------------------------------
%% mod_hexp tests
%%--------------------------------------------------------------------

file_updown_story(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        ImageData = load_test_file(Config),
        FileSize = byte_size(ImageData),


        %%% Upload
        {QueryStanza, ResultStanza} =
        common_upload_request(FileSize, Config, Alice),
        escalus:assert(is_iq_result, [QueryStanza], ResultStanza),
        FileID = do_upload(ResultStanza, ImageData, 200),


        %% Download
        DLQueryStanza = download_stanza(<<"456">>, FileID),
        FinalDLStanza = add_to_from(Config, DLQueryStanza),
        DLResultStanza = escalus:send_and_wait(Alice, FinalDLStanza),
        escalus:assert(is_iq_result, [DLQueryStanza], DLResultStanza),
        ImageData = do_download(DLResultStanza)
    end).

file_up_too_big_story(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        ImageData = load_test_file(Config),
        FileSize = byte_size(ImageData),

        {QueryStanza, ResultStanza} =
        common_upload_request(FileSize div 2, Config, Alice),
        escalus:assert(is_iq_result, [QueryStanza], ResultStanza),
        do_upload(ResultStanza, ImageData, 413)
    end).

file_up_too_small_story(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        ImageData = load_test_file(Config),
        FileSize = byte_size(ImageData),

        {QueryStanza, ResultStanza} =
        common_upload_request(FileSize * 2, Config, Alice),

        escalus:assert(is_iq_result, [QueryStanza], ResultStanza),
        do_upload(ResultStanza, ImageData, 400)
    end).

request_too_big_story(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        {_, ResultStanza} = common_upload_request((1024*1024*10)+1,
                                             Config, Alice),
        escalus:assert(is_iq_error, ResultStanza)
    end).

common_upload_request(Size, Config, User) ->
    QueryStanza = upload_stanza(<<"123">>, <<"image.png">>,
                                Size, <<"image/png">>),
    FinalQueryStanza = add_to_from(Config, QueryStanza),
    ResultStanza = escalus:send_and_wait(User, FinalQueryStanza),
    {QueryStanza, ResultStanza}.

load_test_file(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    ImageFile = filename:join(DataDir, "test_image.png"),
    {ok, ImageData} = file:read_file(ImageFile),
    ImageData.

add_to_from(Config, Stanza) ->
    escalus_stanza:to(
      escalus_stanza:from(Stanza, alice),
      escalus_users:get_server(Config, alice)).

do_upload(ResultStanza, ImageData, ExpectedCode) ->
    UploadEl = get_element(ResultStanza, <<"upload">>),
    URL = get_cdata(UploadEl, <<"url">>),
    Method = get_cdata(UploadEl, <<"method">>),
    HeadersEl = get_element(UploadEl, <<"headers">>),
    FileID = get_cdata(UploadEl, <<"id">>),
    Headers = get_headers(HeadersEl),
    ContentType = proplists:get_value("content-type", Headers),
    FinalHeaders = Headers -- [ContentType],
    {ok, Result} = httpc:request(list_to_atom(
                                   string:to_lower(
                                     binary_to_list(Method))),
                                 {binary_to_list(URL),
                                  FinalHeaders,
                                  ContentType,
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
    true = lists:member({"content-length",
                         integer_to_list(byte_size(RespBin))},
                        RespHeaders),
    true = lists:member({"content-type","image/png"}, RespHeaders),
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

upload_stanza(ID, FileName, Size, Type) ->
    FieldData = [{<<"filename">>, FileName},
                 {<<"size">>, integer_to_list(Size)},
                 {<<"mime-type">>, Type}],
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
