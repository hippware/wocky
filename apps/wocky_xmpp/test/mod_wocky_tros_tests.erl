%%% @copyright 2016+ Hippware, Inc.
%%% @doc Test suite for mod_wocky_tros.erl
-module(mod_wocky_tros_tests).

-compile({parse_transform, cut}).

-include_lib("eunit/include/eunit.hrl").
-include("test_helper.hrl").

-define(MEDIA_FILE, <<"ff9451f2-e712-11e5-9ab0-08002719e96e">>).
-define(FILENAME,   <<"photo of cat.jpg">>).


mod_wocky_tros_test_() -> {
  "mod_wocky_tros",
  inorder,
  [{setup, fun before_all/0, fun after_all/1,
    [
     test_avatar_upload_request(),
     test_message_media_upload_request(),
     test_oversized_upload_request(),
     test_avatar_download_request(),
     test_message_media_download_request(),
     test_bad_download_ids(),
     test_meck_validate()
    ]}
  ]
}.

mecks() -> [?wocky_id, ?tros].

before_all() ->
    lists:foreach(fun(M) -> meck:new(M, [passthrough]) end, mecks()),

    meck:expect(?wocky_id, new, 0, new_file_uuid()),

    _ = ?wocky_repo:delete_all(?wocky_user),
    _ = ?wocky_factory:insert(user, #{id => ?ALICE,
                                      username => ?ALICE,
                                      handle => <<"alice">>}),
    _ = ?wocky_factory:insert(user, #{id => ?BOB,
                                      username => ?BOB,
                                      handle => <<"bob">>}),

    _ = ?wocky_repo:delete_all(?tros_metadata),
    {ok, _} = ?tros_metadata:put(?AVATAR_FILE, ?ALICE, <<"all">>),
    {ok, _} = ?tros_metadata:put(?MEDIA_FILE, ?ALICE,
                                 <<"user:", (?BJID(?BOB))/binary>>),
    ok.

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
               handle_iq(?JID(?ALICE),
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
               handle_iq(?JID(?ALICE),
                         test_server_jid(),
                         upload_packet(10000,
                                       <<"user:",
                                         (?BJID(?BOB))/binary>>
                                      ))))
    ]}]}.

test_oversized_upload_request() ->
    Size = 1024*1024*10 + 1,
    {"Big upload request", [
        {"Oversize request", [
          ?_assertEqual(
             expected_ul_error_packet("Invalid size: 10485761",
                                      <<"all">>, Size),
             handle_iq(?JID(?ALICE),
                       test_server_jid(),
                       upload_packet(Size, <<"all">>)))
    ]}]}.

test_avatar_download_request() ->
    {"Avatar download request", [
        {"Successful request on own avatar using an ID", [
          ?_assert(
             is_expected_download_packet(
               ?AVATAR_FILE,
               handle_iq(?JID(?ALICE),
                         test_server_jid(),
                         download_packet(?AVATAR_FILE))))
    ]},
        {"Successful request on own avatar using a URL", [
          ?_assert(
             is_expected_download_packet(
               ?AVATAR_FILE,
               handle_iq(?JID(?ALICE),
                         test_server_jid(),
                         download_packet(
                           <<"tros:", ?ALICE/binary, "@", ?SERVER/binary,
                             "/file/", ?AVATAR_FILE/binary>>))))
    ]},
        {"Successful request on someone else's avatar", [
          ?_assert(
             is_expected_download_packet(
               ?AVATAR_FILE,
               handle_iq(?JID(?BOB),
                         test_server_jid(),
                         download_packet(?AVATAR_FILE))))
    ]}]}.

test_message_media_download_request() ->
    {"Message media download request", [
        {"Successful request on own media", [
          ?_assert(
             is_expected_download_packet(
               ?MEDIA_FILE,
               handle_iq(?JID(?ALICE),
                         test_server_jid(),
                         download_packet(?MEDIA_FILE))))
    ]},
        {"Successful request on someone else's media that was sent to us", [
          ?_assert(
             is_expected_download_packet(
               ?MEDIA_FILE,
               handle_iq(?JID(?BOB),
                         test_server_jid(),
                         download_packet(?MEDIA_FILE))))
    ]},
        {"Failed request on someone else's media that was NOT sent to us", [
          ?_test(begin
             meck:expect(?tros, get_access, 1,
                         {ok, <<"user:",(?BJID(?BOB))/binary>>}),
             ?assertEqual(
             expected_dl_auth_error_packet(?MEDIA_FILE),
             handle_iq(?JID(?CAROL),
                       test_server_jid(),
                       download_packet(?MEDIA_FILE)))
          end)
    ]}]}.

test_bad_download_ids() ->
    BadUUID = binary:part(?MEDIA_FILE, 0, byte_size(?MEDIA_FILE) - 1),
    BadURL = <<"tros:">>,
    MissingID = ?wocky_id:new(),
    NotFound = expected_dl_missing_error_packet(_),
    {"Bad file ID on download request", [
        {"Failed due to malformed UUID", [
            ?_assertEqual(
               NotFound(BadUUID),
                 handle_iq(?JID(?CAROL),
                           test_server_jid(),
                           download_packet(BadUUID)))
        ]},
        {"Failed due to malformed URL", [
            ?_assertEqual(
               expected_dl_error_packet("Invalid file URL", BadURL),
                 handle_iq(?JID(?CAROL),
                           test_server_jid(),
                           download_packet(BadURL)))
        ]},
        {"Failed due to missing file metadata", [
            ?_assertEqual(
               NotFound(MissingID),
                 handle_iq(?JID(?CAROL),
                           test_server_jid(),
                           download_packet(MissingID)))
        ]}
    ]}.


test_meck_validate() ->
    {"Check that all mecks were called",
      [?_assert(meck:validate(M)) || M <- mecks()]
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

test_server_jid() -> jid:make(<<>>, ?SERVER, <<>>).

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
    JID = <<(?SERVER)/binary, "/file/", UUID/binary>>,
    ID = new_file_uuid(),
    RefURL = <<"tros:", ?ALICE/binary, "@", ?SERVER/binary,
               "/file/", ID/binary>>,
    #xmlel{name = <<"iq">>,
           attrs = [{<<"id">>, <<"123456">>},
                    {<<"type">>, <<"result">>}],
           children =[
             #xmlel{name = <<"upload">>,
                    children =
                    [
                     #xmlel{name = <<"headers">>,
                            attrs = []},
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

expected_dl_missing_error_packet(FileID) ->
    <<"<iq id='123456' type='error'>"
        "<download-request xmlns='", ?NS_TROS/binary, "'>"
          "<id>", FileID/binary, "</id>"
        "</download-request>"
        "<error code='404' type='cancel'>"
          "<item-not-found xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>"
          "<text xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'>"
            "File not found"
          "</text>"
        "</error>"
      "</iq>">>.

expected_dl_auth_error_packet(FileID) ->
    <<"<iq id='123456' type='error'><download-request "
      "xmlns='", ?NS_TROS/binary, "'>"
      "<id>", FileID/binary, "</id></download-request>"
      "<error code='403' type='auth'><forbidden "
      "xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>"
      "<text xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'>"
      "No permission to download this file: permission_denied"
      "</text></error></iq>">>.
