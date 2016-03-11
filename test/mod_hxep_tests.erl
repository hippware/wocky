%%% @copyright 2016+ Hippware, Inc.
%%% @doc Test suite for mod_hxep.erl
-module(mod_hxep_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include("wocky_db_seed.hrl").

mod_hxep_test_() -> {
  "mod_hxep",
  [{setup, fun() -> before_all(Backend) end, fun after_all/1,
    [
     test_avatar_upload_request(Backend),
     test_message_media_upload_request(Backend),
     test_oversized_upload_request(Backend),
     test_avatar_download_request(Backend),
     test_message_media_download_request(Backend),
     test_meck_validate()
    ]}
   || Backend <- [s3, francus]
  ]
}.

mecks() -> [ejabberd_config, ejabberd_sm, httpd_util, ossp_uuid,
            cowboy, mod_hxep_francus, mod_hxep_s3].

before_all(Backend) ->
    {ok, _} = application:ensure_all_started(p1_stringprep),
    ok = wocky_app:start(),

    wocky_db_seed:seed_tables(?LOCAL_CONTEXT, [media]),

    lists:foreach(fun(M) -> meck:new(M, [passthrough]) end, mecks()),
    meck:expect(ejabberd_config, add_local_option, 2, {atomic, ok}),
    meck:expect(ejabberd_config, get_local_option,
                fun(hxep_backend) -> Backend;
                   (s3_bucket) -> "hxep-test";
                   (s3_access_key_id) -> "AKIAI4OZWBAA4SP6Y3WA";
                   (s3_secret_key) ->
                        "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";
                   (hxep_max_upload_size) -> 1024 * 1024 * 10
                end),
    meck:expect(ejabberd_config, get_local_option,
                fun(cqerl_node, _) -> {"127.0.0.1", 9042}
                end),
    meck:expect(ejabberd_config, get_global_option,
                fun(francus_chunk_size) -> undefined; % Use the default
                   (language) -> <<"en">>;
                   (hosts) -> [?LOCAL_CONTEXT]
                end),

    meck:expect(gen_iq_handler, add_iq_handler, 6, ok),
    meck:expect(gen_iq_handler, remove_iq_handler, 3, ok),

    meck:expect(httpd_util, rfc1123_date, 1,
                "Fri, 29 Jan 2016 02:54:44 GMT"),

    meck:expect(ossp_uuid, make, 2, new_file_uuid()),
    % Don't seem to be able to do meck passthrough to NIFs :(
    % So we reimplement it here by calling a non-NIF version:
    meck:expect(ossp_uuid, import, fun(UUID, text) ->
                                           list_to_binary(
                                             uuid:uuid_to_string(UUID)) end),

    meck:expect(cowboy, start_http, 4, ok),

    meck:expect(mod_hxep_francus, make_auth,
                fun() -> base64:encode(binary:copy(<<6:8>>, 48)) end),

    meck:expect(mod_hxep_s3, make_auth, 5,
                base64:encode(binary:copy(<<6:8>>, 48))),

    mod_hxep:start(?LOCAL_CONTEXT, []).

after_all(_) ->
    mod_hxep:stop(?LOCAL_CONTEXT),
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
                                     avatar_data(?ALICE)
                                    )))
    ]},
        {"Rejected request when uploading an avatar for someone else", [
          ?_assertEqual(
             expected_ul_error_packet("Permission denied", "avatar",
                                      avatar_data(?BOB), 10000),
             handle_iq(test_user_jid(?ALICE),
                       test_server_jid(),
                       upload_packet(10000,
                                     "avatar",
                                     avatar_data(?BOB)
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

test_oversized_upload_request(_Backend) ->
    Size = 1024*1024*10 + 1,
    {"Big upload request", [
        {"Oversize request", [
          ?_assertEqual(
             expected_ul_error_packet("Invalid file size", "avatar",
                                      avatar_data(?ALICE), Size),
             handle_iq(test_user_jid(?ALICE),
                       test_server_jid(),
                       upload_packet(Size,
                                     "avatar",
                                     avatar_data(?ALICE)
                                    )))
    ]}]}.

test_avatar_download_request(Backend) ->
    {"Avatar download request", [
        {"Successful request on own avatar", [
          ?_assertEqual(
             expected_download_packet(Backend, ?AVATAR_FILE),
             handle_iq(test_user_jid(?ALICE),
                       test_server_jid(),
                       download_packet(?AVATAR_FILE)))
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
             expected_dl_error_packet("Permission denied: permission_denied",
                                      ?MEDIA_FILE),
             handle_iq(test_user_jid(?CAROL),
                       test_server_jid(),
                       download_packet(?MEDIA_FILE)))
    ]}]}.

test_meck_validate() ->
    {"Check that all mecks were called", [
        ?_assert(meck:validate(M)) || M <- mecks()]
    }.


%%==============================================================================
%% Helpers
%%==============================================================================

handle_iq(FromJID, ServerJID, Packet) ->
    exml:to_binary(
      jlib:iq_to_xml(
        mod_hxep:handle_iq(FromJID, ServerJID, Packet))).

new_file_uuid() ->
    <<"a65ecb4e-c633-11e5-9fdc-080027f70e96">>.

test_user_jid(User) -> jid:make(User, ?LOCAL_CONTEXT, ?RESOURCE).

test_server_jid() -> jid:make(<<>>, ?LOCAL_CONTEXT, <<>>).

common_packet(Type, Request) ->
    #iq{id = <<"123456">>,
        type = Type,
        sub_el = Request
       }.

upload_packet(Size, Type, TypeData) ->
    common_packet(set, upload_request(Size, Type, TypeData)).

upload_request(Size, Type, TypeData) ->
    Elements = [{<<"filename">>, <<"photo.jpeg">>},
                {<<"size">>, integer_to_binary(Size)},
                {<<"mime-type">>, <<"image/jpeg">>},
                {<<"purpose">>, <<(list_to_binary(Type))/binary, ":",
                                  TypeData/binary>>}
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

avatar_data(User) -> jid:to_binary(jid:to_bare(test_user_jid(User))).

expected_upload_packet(s3) ->
    <<"<iq id='123456' type='result'><upload><headers>"
      "<header name='host' value='hxep-test.s3.amazonaws.com'/>"
      "<header name='content-type' value='image/jpeg'/>"
      "<header name='date' value='Fri, 29 Jan 2016 02:54:44 GMT'/>"
      "<header name='authorization' value="
      "'BgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYG'"
      "/></headers>"
      "<id>a65ecb4e-c633-11e5-9fdc-080027f70e96</id>"
      "<jid>", ?LOCAL_CONTEXT/binary, "/file/", (new_file_uuid())/binary,
      "</jid><url>https://hxep-test.s3.amazonaws.com/", ?ALICE/binary, "/",
      (new_file_uuid())/binary, "</url><method>PUT</method>"
      "</upload></iq>">>;

expected_upload_packet(francus) ->
    <<"<iq id='123456' type='result'><upload><headers>"
      "<header name='content-type' value='image/jpeg'/>"
      "<header name='authorization' value="
      "'BgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYG'"
      "/></headers>"
      "<id>a65ecb4e-c633-11e5-9fdc-080027f70e96</id>"
      "<jid>", ?LOCAL_CONTEXT/binary, "/file/",
      (new_file_uuid())/binary, "</jid>"
      "<url>https://", ?LOCAL_CONTEXT/binary,
      ":1025/users/", ?ALICE/binary, "/",
      "files/", (new_file_uuid())/binary, "</url>"
      "<method>PUT</method></upload></iq>">>.

expected_download_packet(s3, FileID) ->
    <<"<iq id='123456' type='result'><download><headers>"
      "<header name='host' value='hxep-test.s3.amazonaws.com'/>"
      "<header name='date' value='Fri, 29 Jan 2016 02:54:44 GMT'/>"
      "<header name='authorization' value="
      "'BgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYG'"
      "/></headers>"
      "<url>https://hxep-test.s3.amazonaws.com/", ?ALICE/binary, "/",
      FileID/binary, "</url></download></iq>">>;

expected_download_packet(francus, FileID) ->
    <<"<iq id='123456' type='result'><download><headers>"
      "<header name='authorization' value="
      "'BgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYGBgYG'"
      "/></headers>"
      "<url>https://", ?LOCAL_CONTEXT/binary, ":1025/users/", ?ALICE/binary,
      "/files/", FileID/binary, "</url>"
      "<method>GET</method></download></iq>">>.

expected_ul_error_packet(Reason, Type, TypeData, Size) ->
    <<"<iq id='123456' type='error'><upload-request xmlns='hippware.com/"
      "hxep/http-file'><filename>photo.jpeg</filename><size>",
      (integer_to_binary(Size))/binary, "</size>"
      "<mime-type>image/jpeg</mime-type>"
      "<purpose>", (list_to_binary(Type))/binary, ":",
      TypeData/binary, "</purpose></upload-request>"
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
      (list_to_binary(Reason))/binary, "</text></error></iq>">>.
