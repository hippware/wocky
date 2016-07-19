%%% @copyright 2016+ Hippware, Inc.
%%% @doc Test suite for mod_tros.erl
-module(mod_tros_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include("wocky_db_seed.hrl").

mod_tros_test_() -> {
  "mod_tros",
  [{setup, fun() -> before_all(Backend) end, fun after_all/1,
    [
     test_avatar_upload_request(Backend),
     test_message_media_upload_request(Backend),
     test_group_chat_media_upload_request(Backend),
     test_oversized_upload_request(Backend),
     test_avatar_download_request(Backend),
     test_message_media_download_request(Backend),
     test_group_chat_media_download_request(Backend),
     test_bad_download_ids(Backend),
     test_meck_validate()
    ]}
   || Backend <- [s3, francus]
  ]
}.

mecks() -> [ejabberd_config, httpd_util, ossp_uuid,
            mod_tros_francus, mod_tros_s3].

before_all(Backend) ->
    lists:foreach(fun(M) -> meck:new(M, [passthrough]) end, mecks()),
    meck:expect(ejabberd_config, get_local_option,
                fun(tros_backend) -> Backend;
                   (s3_bucket) -> "tros-test";
                   (s3_access_key_id) -> "AKIAI4OZWBAA4SP6Y3WA";
                   (s3_secret_key) ->
                        "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";
                   (tros_max_upload_size) -> 1024 * 1024 * 10;
                   (tros_scheme) -> "http://";
                   (tros_auth_validity) -> 3600;
                   (tros_port) -> 1025;
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

    meck:expect(mod_tros_francus, make_auth,
                fun() -> base64:encode(binary:copy(<<6:8>>, 48)) end),

    meck:expect(mod_tros_s3, make_auth, 5,
                base64:encode(binary:copy(<<6:8>>, 48))),
    meck:expect(mod_tros_s3, get_owner, 2, {ok, ?ALICE}),
    meck:expect(mod_tros_s3, get_metadata,
                fun(A, B) -> mod_tros_francus:get_metadata(A, B) end),

    ok = wocky_app:start(),
    wocky_db_seed:prepare_tables(?LOCAL_CONTEXT, [media, tros_request,
                                                  group_chat]),
    wocky_db_seed:seed_tables(?LOCAL_CONTEXT, [media, tros_request,
                                               group_chat]).


after_all(_) ->
    ok = wocky_app:stop(),
    meck:unload().


%%==============================================================================
%% HXEP tests
%%==============================================================================

test_avatar_upload_request(Backend) ->
    {"Upload request", [
        {"Successful request", [
          ?_assertEqual(
             expected_upload_packet(Backend),
             handle_iq(test_user_jid(?ALICE),
                       test_server_jid(),
                       upload_packet(10000,
                                     "avatar",
                                     <<>>
                                    )))
    ]}]}.

test_message_media_upload_request(Backend) ->
    { "Message media upload request", [
        {"Successful request", [
          ?_assertEqual(
             expected_upload_packet(Backend),
             handle_iq(test_user_jid(?ALICE),
                       test_server_jid(),
                       upload_packet(10000,
                                     "message_media",
                                     jid:to_binary(
                                       test_user_jid(?BOB))
                                    )))
    ]}]}.

test_group_chat_media_upload_request(Backend) ->
    { "Group chat media upload request", [
        {"Successful request when we're a group member", [
          ?_assertEqual(
             expected_upload_packet(Backend),
             handle_iq(test_user_jid(?ALICE),
                       test_server_jid(),
                       upload_packet(10000,
                                     "group_chat_media",
                                     jid:to_binary(
                                       ?GROUP_CHAT_JID)
                                    )))
        ]},
        {"Failed request when we're not a group member", [
          ?_assertEqual(
             expected_ul_error_packet("Permission denied", "group_chat_media",
                                      ?GROUP_CHAT, 10000),
             handle_iq(test_user_jid(?CAROL),
                       test_server_jid(),
                       upload_packet(10000,
                                     "group_chat_media",
                                     ?GROUP_CHAT)
                                    ))
    ]}]}.

test_oversized_upload_request(_Backend) ->
    Size = 1024*1024*10 + 1,
    {"Big upload request", [
        {"Oversize request", [
          ?_assertEqual(
             expected_ul_error_packet("Invalid size: 10485761", "avatar",
                                      <<>>, Size),
             handle_iq(test_user_jid(?ALICE),
                       test_server_jid(),
                       upload_packet(Size,
                                     "avatar",
                                     <<>>
                                    )))
    ]}]}.

test_avatar_download_request(Backend) ->
    {"Avatar download request", [
        {"Successful request on own avatar using an ID", [
          ?_assertEqual(
             expected_download_packet(Backend, ?AVATAR_FILE),
             handle_iq(test_user_jid(?ALICE),
                       test_server_jid(),
                       download_packet(?AVATAR_FILE)))
    ]},
        {"Successful request on own avatar using a URL", [
          ?_assertEqual(
             expected_download_packet(Backend, ?AVATAR_FILE),
             handle_iq(test_user_jid(?ALICE),
                       test_server_jid(),
                       download_packet(
                         <<"tros:", ?ALICE/binary, "@", ?LOCAL_CONTEXT/binary,
                           "/file/", ?AVATAR_FILE/binary>>)))
    ]},
        {"Successful request on someone else's avatar", [
          ?_assertEqual(
             expected_download_packet(Backend, ?AVATAR_FILE),
             handle_iq(test_user_jid(?BOB),
                       test_server_jid(),
                       download_packet(?AVATAR_FILE)))
    ]}]}.

test_message_media_download_request(Backend) ->
    {"Message media download request", [
        {"Successful request on own media", [
          ?_assertEqual(
             expected_download_packet(Backend, ?MEDIA_FILE),
             handle_iq(test_user_jid(?ALICE),
                       test_server_jid(),
                       download_packet(?MEDIA_FILE)))
    ]},
        {"Successful request on someone else's media that was sent to us", [
          ?_assertEqual(
             expected_download_packet(Backend, ?MEDIA_FILE),
             handle_iq(test_user_jid(?BOB),
                       test_server_jid(),
                       download_packet(?MEDIA_FILE)))
    ]},
        {"Failed request on someone else's media that was NOT sent to us", [
          ?_assertEqual(
             expected_dl_auth_error_packet(?MEDIA_FILE),
             handle_iq(test_user_jid(?CAROL),
                       test_server_jid(),
                       download_packet(?MEDIA_FILE)))
    ]}]}.

test_group_chat_media_download_request(Backend) ->
    {"Group chat media download request", [
        {"Successful request on own media", [
          ?_assertEqual(
             expected_download_packet(Backend, ?GC_MEDIA_FILE),
             handle_iq(test_user_jid(?ALICE),
                       test_server_jid(),
                       download_packet(?GC_MEDIA_FILE)))
    ]},
        {"Successful request on media in the same group chat as us", [
          ?_assertEqual(
             expected_download_packet(Backend, ?GC_MEDIA_FILE),
             handle_iq(test_user_jid(?BOB),
                       test_server_jid(),
                       download_packet(?GC_MEDIA_FILE)))
    ]},
        {"Failed request on media for a group chat we're not in", [
          ?_assertEqual(
             expected_dl_auth_error_packet(?GC_MEDIA_FILE),
             handle_iq(test_user_jid(?CAROL),
                       test_server_jid(),
                       download_packet(?GC_MEDIA_FILE)))
    ]}]}.

test_bad_download_ids(_Backend) ->
    BadUUID = binary:part(?MEDIA_FILE, 0, byte_size(?MEDIA_FILE) - 1),
    BadURL = <<"tros:">>,
    MissingID = wocky_db:create_id(),
    {"Bad file ID on download request", [
        {"Failed due to malformed UUID", [
            ?_assertEqual(
               expected_dl_missing_error_packet(BadUUID),
                 handle_iq(test_user_jid(?CAROL),
                           test_server_jid(),
                           download_packet(BadUUID)))
        ]},
        {"Failed due to malformed URL", [
            ?_assertEqual(
               expected_dl_error_packet("Invalid file URL", BadURL),
                 handle_iq(test_user_jid(?CAROL),
                           test_server_jid(),
                           download_packet(BadURL)))
        ]},
        {"Failed due to missing file metadata", [
            ?_assertEqual(
               expected_dl_missing_error_packet(MissingID),
                 handle_iq(test_user_jid(?CAROL),
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
        mod_tros:handle_iq(FromJID, ServerJID, Packet))).

new_file_uuid() ->
    <<"a65ecb4e-c633-11e5-9fdc-080027f70e96">>.

test_user_jid(User) -> jid:make(User, ?LOCAL_CONTEXT, ?RESOURCE).

test_server_jid() -> jid:make(<<>>, ?LOCAL_CONTEXT, <<>>).

common_packet(Type, Request) ->
    #iq{id = <<"123456">>,
        type = Type,
        sub_el = Request
       }.

upload_packet(Size, Type, Access) ->
    common_packet(set, upload_request(Size, Type, Access)).

upload_request(Size, Type, Access) ->
    Elements = [{<<"filename">>, ?FILENAME},
                {<<"size">>, integer_to_binary(Size)},
                {<<"mime-type">>, <<"image/jpeg">>},
                {<<"purpose">>, <<(list_to_binary(Type))/binary>>},
                {<<"access">>, <<Access/binary>>}
               ],
    #xmlel{name = <<"upload-request">>,
           attrs = [{<<"xmlns">>, <<"hippware.com/hxep/http-file">>}],
           children = [
                       #xmlel{name = N,
                              children = [#xmlcdata{content = C}]}
                       || {N, C} <- Elements]}.

download_packet(FileID) ->
    common_packet(get, download_request(FileID)).

download_request(FileID) ->
    #xmlel{name = <<"download-request">>,
           attrs = [{<<"xmlns">>, <<"hippware.com/hxep/http-file">>}],
           children = [#xmlel{name = <<"id">>,
                              children = [#xmlcdata{content = FileID}]}]}.

expected_upload_packet(s3) ->
    <<"<iq id='123456' type='result'><upload><headers>"
      "<header name='host' value='tros-test.s3.amazonaws.com'/>"
      "<header name='content-type' value='image/jpeg'/>"
      "<header name='date' value='Fri, 29 Jan 2016 02:54:44 GMT'/>"
      "<header name='authorization' value="
      "'BgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYG'"
      "/></headers>"
      "<id>", (new_file_uuid())/binary, "</id>"
      "<jid>", ?LOCAL_CONTEXT/binary, "/file/", (new_file_uuid())/binary,
      "</jid><url>https://tros-test.s3.amazonaws.com/", ?ALICE/binary, "/",
      (new_file_uuid())/binary, "</url><method>PUT</method>"
      "</upload></iq>">>;

expected_upload_packet(francus) ->
    <<"<iq id='123456' type='result'><upload><headers>"
      "<header name='content-type' value='image/jpeg'/>"
      "<header name='authorization' value="
      "'BgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYG'"
      "/></headers>"
      "<id>", (new_file_uuid())/binary, "</id>"
      "<jid>", ?LOCAL_CONTEXT/binary, "/file/",
      (new_file_uuid())/binary, "</jid>"
      "<url>http://", ?LOCAL_CONTEXT/binary,
      ":1025/users/", ?ALICE/binary, "/",
      "files/", (new_file_uuid())/binary, "</url>"
      "<method>POST</method>"
      "<reference_url>tros:", ?ALICE/binary, "@", ?LOCAL_CONTEXT/binary,
      "/file/", (new_file_uuid())/binary, "</reference_url>"
      "</upload></iq>">>.

expected_download_packet(s3, FileID) ->
    <<"<iq id='123456' type='result'><download><headers>"
      "<header name='host' value='tros-test.s3.amazonaws.com'/>"
      "<header name='date' value='Fri, 29 Jan 2016 02:54:44 GMT'/>"
      "<header name='authorization' value="
      "'BgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYG'"
      "/></headers>"
      "<url>https://tros-test.s3.amazonaws.com/", ?ALICE/binary, "/",
      FileID/binary, "</url></download></iq>">>;

expected_download_packet(francus, FileID) ->
    <<"<iq id='123456' type='result'><download><headers>"
      "<header name='authorization' value="
      "'BgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYG'"
      "/></headers>"
      "<url>http://", ?LOCAL_CONTEXT/binary, ":1025/users/", ?ALICE/binary,
      "/files/", FileID/binary, "/", ?URL_FILENAME/binary, "</url>"
      "<method>GET</method></download></iq>">>.

expected_ul_error_packet(Reason, Type, Access, Size) ->
    <<"<iq id='123456' type='error'><upload-request xmlns='hippware.com/"
      "hxep/http-file'><filename>", ?FILENAME/binary, "</filename><size>",
      (integer_to_binary(Size))/binary, "</size>"
      "<mime-type>image/jpeg</mime-type>"
      "<purpose>", (list_to_binary(Type))/binary, "</purpose>",
      "<access>", Access/binary, "</access>",
      "</upload-request>"
      "<error code='406' type='modify'><not-acceptable "
      "xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>"
      "<text xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'>",
      "Upload request denied: ",
      (list_to_binary(Reason))/binary, "</text></error></iq>">>.

expected_dl_error_packet(Reason, FileID) ->
    <<"<iq id='123456' type='error'><download-request "
      "xmlns='hippware.com/hxep/http-file'>"
      "<id>", FileID/binary, "</id></download-request>"
      "<error code='406' type='modify'><not-acceptable "
      "xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>"
      "<text xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'>"
      "Download request denied: ",
      (iolist_to_binary(Reason))/binary, "</text></error></iq>">>.

expected_dl_missing_error_packet(FileID) ->
    <<"<iq id='123456' type='error'>"
        "<download-request xmlns='hippware.com/hxep/http-file'>"
          "<id>", FileID/binary, "</id>"
        "</download-request>"
        "<error code='404' type='cancel'>"
          "<item-not-found xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>"
          "<text xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'>"
            "File metadata not found"
          "</text>"
        "</error>"
      "</iq>">>.

expected_dl_auth_error_packet(FileID) ->
    <<"<iq id='123456' type='error'><download-request "
      "xmlns='hippware.com/hxep/http-file'>"
      "<id>", FileID/binary, "</id></download-request>"
      "<error code='403' type='auth'><forbidden "
      "xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>"
      "<text xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'>"
      "No permission to download this file: permission_denied"
      "</text></error></iq>">>.
