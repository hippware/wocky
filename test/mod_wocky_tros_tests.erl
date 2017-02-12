%%% @copyright 2016+ Hippware, Inc.
%%% @doc Test suite for mod_wocky_tros.erl
-module(mod_wocky_tros_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include("wocky_db_seed.hrl").
-include("wocky.hrl").

-compile({parse_transform, cut}).

mod_wocky_tros_test_() -> {
  "mod_wocky_tros",
  inorder,
  [{setup, fun before_all/0, fun after_all/1,
    [
     test_avatar_upload_request(),
     test_message_media_upload_request(),
     test_group_chat_media_upload_request(),
     test_oversized_upload_request(),
     test_avatar_download_request(),
     test_message_media_download_request(),
     test_group_chat_media_download_request(),
     test_bad_download_ids(),
     test_meck_validate()
    ]}
  ]
}.

mecks() -> [ejabberd_config, httpd_util, ossp_uuid, mod_wocky_tros_s3].

before_all() ->
    lists:foreach(fun(M) -> meck:new(M, [passthrough]) end, mecks()),
    meck:expect(ejabberd_config, get_local_option,
                fun(tros_backend) -> s3;
                   (tros_scheme) -> "http://";
                   (tros_auth_validity) -> 3600;
                   (s3_bucket) -> "tros-test";
                   (s3_access_key_id) -> "AKIAI4OZWBAA4SP6Y3WA";
                   (s3_secret_key) ->
                        "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";
                   (tros_public_port) -> 1025;
                   (X) -> meck:passthrough([X])
                end),
    meck:expect(httpd_util, rfc1123_date, 1,
                "Fri, 29 Jan 2016 02:54:44 GMT"),

    meck:expect(ossp_uuid, make, 2, new_file_uuid()),
    % Don't seem to be able to do meck passthrough to NIFs :(
    % So we reimplement it here by calling a non-NIF version:
    meck:expect(ossp_uuid, import, fun(UUID, text) ->
                                           list_to_binary(
                                             uuid:uuid_to_string(UUID));
                                      (UUID, binary) ->
                                             uuid:string_to_uuid(UUID)
                                   end),

    meck:expect(mod_wocky_tros_s3, get_access, 1, {ok, <<"all">>}),
    meck:expect(mod_wocky_tros_s3, get_metadata, 2,
                {ok,
                 #{<<"content-type">> => <<"image/png">>,
                   <<"x-amz-meta-name">> => <<"photo of cat.jpg">>,
                   <<"x-amz-meta-owner">> =>
                     <<"043e8c96-ba30-11e5-9912-ba0be0483c18">>}}),

    wocky_db_seed:seed_tables(?LOCAL_CONTEXT, [media, tros_request,
                                               group_chat]).


after_all(_) ->
    meck:unload().


%%==============================================================================
%% TROS tests
%%==============================================================================

test_avatar_upload_request() ->
    {"Upload request", [
        {"Successful request", [
          ?_assert(
             is_expected_upload_packet(
               handle_iq(?ALICE_JID,
                         test_server_jid(),
                         upload_packet(10000,
                                       <<"all">>
                                      ))))
    ]}]}.

test_message_media_upload_request() ->
    { "Message media upload request", [
        {"Successful request", [
          ?_assert(
             is_expected_upload_packet(
               handle_iq(?ALICE_JID,
                         test_server_jid(),
                         upload_packet(10000,
                                       <<"user:",
                                         (?BOB_B_JID)/binary>>
                                      ))))
    ]}]}.

test_group_chat_media_upload_request() ->
    { "Group chat media upload request", [
        {"Successful request when we're a group member", [
          ?_assert(
             is_expected_upload_packet(
               handle_iq(?ALICE_JID,
                         test_server_jid(),
                         upload_packet(10000,
                                       <<"members:",
                                         (jid:to_binary(
                                            ?GROUP_CHAT_JID))/binary>>
                                      ))))
    ]}]}.

test_oversized_upload_request() ->
    Size = 1024*1024*10 + 1,
    {"Big upload request", [
        {"Oversize request", [
          ?_assertEqual(
             expected_ul_error_packet("Invalid size: 10485761",
                                      <<"all">>, Size),
             handle_iq(?ALICE_JID,
                       test_server_jid(),
                       upload_packet(Size, <<"all">>)))
    ]}]}.

test_avatar_download_request() ->
    {"Avatar download request", [
        {"Successful request on own avatar using an ID", [
          ?_assert(
             is_expected_download_packet(
               ?AVATAR_FILE,
               handle_iq(?ALICE_JID,
                         test_server_jid(),
                         download_packet(?AVATAR_FILE))))
    ]},
        {"Successful request on own avatar using a URL", [
          ?_assert(
             is_expected_download_packet(
               ?AVATAR_FILE,
               handle_iq(?ALICE_JID,
                         test_server_jid(),
                         download_packet(
                           <<"tros:", ?ALICE/binary, "@", ?LOCAL_CONTEXT/binary,
                             "/file/", ?AVATAR_FILE/binary>>))))
    ]},
        {"Successful request on someone else's avatar", [
          ?_assert(
             is_expected_download_packet(
               ?AVATAR_FILE,
               handle_iq(?BOB_JID,
                         test_server_jid(),
                         download_packet(?AVATAR_FILE))))
    ]}]}.

test_message_media_download_request() ->
    {"Message media download request", [
        {"Successful request on own media", [
          ?_assert(
             is_expected_download_packet(
               ?MEDIA_FILE,
               handle_iq(?ALICE_JID,
                         test_server_jid(),
                         download_packet(?MEDIA_FILE))))
    ]},
        {"Successful request on someone else's media that was sent to us", [
          ?_assert(
             is_expected_download_packet(
               ?MEDIA_FILE,
               handle_iq(?BOB_JID,
                         test_server_jid(),
                         download_packet(?MEDIA_FILE))))
    ]},
        {"Failed request on someone else's media that was NOT sent to us", [
          ?_test(begin
             meck:expect(mod_wocky_tros_s3, get_access, 1,
                         {ok, <<"user:",(?BOB_B_JID)/binary>>}),
             ?assertEqual(
             expected_dl_auth_error_packet(?MEDIA_FILE),
             handle_iq(?CAROL_JID,
                       test_server_jid(),
                       download_packet(?MEDIA_FILE)))
          end)
    ]}]}.

test_group_chat_media_download_request() ->
    {"Group chat media download request", [
        {"Successful request on own media", [
          ?_assert(
             is_expected_download_packet(
               ?GC_MEDIA_FILE,
               handle_iq(?ALICE_JID,
                         test_server_jid(),
                         download_packet(?GC_MEDIA_FILE))))
    ]},
        {"Successful request on media in the same group chat as us", [
          ?_assert(
             is_expected_download_packet(
               ?GC_MEDIA_FILE,
               handle_iq(?BOB_JID,
                         test_server_jid(),
                         download_packet(?GC_MEDIA_FILE))))
    ]},
        {"Failed request on media for a group chat we're not in", [
          ?_assertEqual(
             expected_dl_auth_error_packet(?GC_MEDIA_FILE),
             handle_iq(?CAROL_JID,
                       test_server_jid(),
                       download_packet(?GC_MEDIA_FILE)))
    ]}]}.

test_bad_download_ids() ->
    BadUUID = binary:part(?MEDIA_FILE, 0, byte_size(?MEDIA_FILE) - 1),
    BadURL = <<"tros:">>,
    MissingID = wocky_db:create_id(),
    NotFound = expected_dl_auth_error_packet(_),
    {"Bad file ID on download request", [
        {"Failed due to malformed UUID", [
            ?_assertEqual(
               NotFound(BadUUID),
                 handle_iq(?CAROL_JID,
                           test_server_jid(),
                           download_packet(BadUUID)))
        ]},
        {"Failed due to malformed URL", [
            ?_assertEqual(
               expected_dl_error_packet("Invalid file URL", BadURL),
                 handle_iq(?CAROL_JID,
                           test_server_jid(),
                           download_packet(BadURL)))
        ]},
        {"Failed due to missing file metadata", [
            ?_assertEqual(
               NotFound(MissingID),
                 handle_iq(?CAROL_JID,
                           test_server_jid(),
                           download_packet(MissingID)))
        ]}
    ]}.


test_meck_validate() ->
    {"Check that all mecks were called", [
        % Exclude ossp_uuid since we *expect* some of the calls to it to
        % throw exceptions, since that's how we detect invalid UUIDs
        ?_assert(meck:validate(M)) || M <- mecks() -- [ossp_uuid]]
    }.


%%==============================================================================
%% Helpers
%%==============================================================================

handle_iq(FromJID, ServerJID, Packet) ->
    exml:to_binary(
      jlib:iq_to_xml(
        mod_wocky_tros:handle_iq(FromJID, ServerJID, Packet))).

new_file_uuid() ->
    <<"a65ecb4e-c633-11e5-9fdc-080027f70e96">>.

test_server_jid() -> jid:make(<<>>, ?LOCAL_CONTEXT, <<>>).

common_packet(Type, Request) ->
    #iq{id = <<"123456">>,
        type = Type,
        sub_el = Request
       }.

upload_packet(Size, Access) ->
    common_packet(set, upload_request(Size, Access)).

upload_request(Size, Access) ->
    Elements = [{<<"filename">>, ?FILENAME},
                {<<"size">>, integer_to_binary(Size)},
                {<<"mime-type">>, <<"image/jpeg">>},
                {<<"access">>, <<Access/binary>>}
               ],
    #xmlel{name = <<"upload-request">>,
           attrs = [{<<"xmlns">>, ?NS_TROS}],
           children = [
                       #xmlel{name = N,
                              children = [#xmlcdata{content = C}]}
                       || {N, C} <- Elements]}.

download_packet(FileID) ->
    common_packet(get, download_request(FileID)).

download_request(FileID) ->
    #xmlel{name = <<"download-request">>,
           attrs = [{<<"xmlns">>, ?NS_TROS}],
           children = [#xmlel{name = <<"id">>,
                              children = [#xmlcdata{content = FileID}]}]}.

is_expected_upload_packet(P) ->
    {ok, XML} = exml:parse(P),
    UUID = new_file_uuid(),
    JID = <<(?LOCAL_CONTEXT)/binary, "/file/", UUID/binary>>,
    ID = new_file_uuid(),
    RefURL = <<"tros:", ?ALICE/binary, "@", ?LOCAL_CONTEXT/binary,
               "/file/", ID/binary>>,
    #xmlel{name = <<"iq">>,
           attrs = [{<<"id">>, <<"123456">>},
                    {<<"type">>, <<"result">>}],
           children =[
             #xmlel{name = <<"upload">>,
                    children =
                    [
                     #xmlel{name = <<"headers">>,
                            attrs = [],
                            children = [#xmlel{name = <<"header">>,
                                               attrs = [{<<"name">>,
                                                         <<"content-type">>},
                                                        {<<"value">>,
                                                         <<"image/jpeg">>}]}]},
                     #xmlel{name = <<"id">>,
                            attrs = [],
                            children = [#xmlcdata{content = UUID}]},
                     #xmlel{name = <<"jid">>,
                            attrs = [],
                            children = [#xmlcdata{content = JID}]},
                     #xmlel{name = <<"method">>,
                            attrs = [],
                            children = [#xmlcdata{content = <<"PUT">>}]},
                     #xmlel{name = <<"url">>,
                            attrs = []},
                     #xmlel{name = <<"reference_url">>,
                            attrs = [],
                            children = [#xmlcdata{content = RefURL}]}
                    ]}]}
    = XML,
    true.

is_expected_download_packet(_FileID, P) ->
    {ok, XML} = exml:parse(P),
    #xmlel{name = <<"iq">>,
           attrs = [{<<"id">>, <<"123456">>},
                    {<<"type">>, <<"result">>}],
           children =[
             #xmlel{name = <<"download">>,
                    children =
                    [
                     #xmlel{name = <<"headers">>,
                            attrs = [],
                            children = []},
                     #xmlel{name = <<"url">>,
                            attrs = []}
                    ]}]}
    = XML,
    true.

expected_ul_error_packet(Reason, Access, Size) ->
    <<"<iq id='123456' type='error'><upload-request xmlns='", ?NS_TROS/binary,
      "'><filename>", ?FILENAME/binary, "</filename><size>",
      (integer_to_binary(Size))/binary, "</size>"
      "<mime-type>image/jpeg</mime-type>"
      "<access>", Access/binary, "</access>",
      "</upload-request>"
      "<error code='406' type='modify'><not-acceptable "
      "xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>"
      "<text xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'>",
      "Upload request denied: ",
      (list_to_binary(Reason))/binary, "</text></error></iq>">>.

expected_dl_error_packet(Reason, FileID) ->
    <<"<iq id='123456' type='error'><download-request "
      "xmlns='", ?NS_TROS/binary, "'>"
      "<id>", FileID/binary, "</id></download-request>"
      "<error code='406' type='modify'><not-acceptable "
      "xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>"
      "<text xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'>"
      "Download request denied: ",
      (iolist_to_binary(Reason))/binary, "</text></error></iq>">>.

expected_dl_auth_error_packet(FileID) ->
    <<"<iq id='123456' type='error'><download-request "
      "xmlns='", ?NS_TROS/binary, "'>"
      "<id>", FileID/binary, "</id></download-request>"
      "<error code='403' type='auth'><forbidden "
      "xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>"
      "<text xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'>"
      "No permission to download this file: permission_denied"
      "</text></error></iq>">>.
