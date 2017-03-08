%%% @copyright 2016+ Hippware, Inc.
%%% @doc Macros useful for seed data used in tests

-ifndef(WOCKY_DB_SEED_HRL).
-define(WOCKY_DB_SEED_HRL, 1).

-include("wocky.hrl").

-define(LOCAL_CONTEXT, <<"localhost">>).

-define(USER,        <<"043e8c96-ba30-11e5-9912-ba0be0483c18">>).
-define(SERVER,      <<"localhost">>).
-define(RESOURCE,    <<"testing">>).
-define(HANDLE,      <<"alice">>).
-define(PHONE_NUMBER,<<"+1234">>).
-define(AVATAR_ID,   <<"tros:8b70de6c-e6fe-11e5-9730-9a79f06e9478@localhost">>).
-define(EXTERNAL_ID, <<"701990807448920064">>).
-define(PASS,        <<"password">>).
-define(SCRAM,       <<"==SCRAM==,8dncGDJ45eXoMzqZ3zT6F/8gJt0=,"
                       "aEsAiUKWIdvXDW5oSaQxUJKHcOU=,Q0t9V/6+rkA1jv0LGA/IHw==,"
                       "4096">>).
-define(TOKEN,       <<"$T$QU15BwZioTHgSlcIk6D0odmfaXirL9ajzu7qeeFkAuU=">>).
-define(DIGITS_AUTH,
        <<"OAuth oauth_signature=\"32%2Bf2UY9txk%2BIcV0mV7P55R9%2Fkw%3D\","
          "oauth_nonce=\"EF4F602C-0BBF-48F1-91C7-7CD344E7D0A1\","
          "oauth_timestamp=\"1456795178\","
          "oauth_consumer_key=\"e527IQiWSXZ5WHNxROUZk87uV\","
          "oauth_token=\"701990807448920064-JxNX4i57y5Wp6xBDVjNwKB4ZYUcC8FJ\","
          "oauth_version=\"1.0\",oauth_signature_method=\"HMAC-SHA1\"">>).

-define(BADUSER, <<"d51f92c8-ba40-11e5-9912-ba0be0483c18">>).
-define(NEWUSER, <<"31a07b20-c5fe-11e5-9912-ba0be0483c18">>).

-define(ALICE,  <<"043e8c96-ba30-11e5-9912-ba0be0483c18">>).
-define(CAROL,  <<"1e6f3b36-c5fe-11e5-9912-ba0be0483c18">>).
-define(BOB,    <<"2396aa72-c5fe-11e5-9912-ba0be0483c18">>).
-define(KAREN,  <<"2841ef1e-c5fe-11e5-9912-ba0be0483c18">>).
-define(ROBERT, <<"2c33f32e-c5fe-11e5-9912-ba0be0483c18">>).
-define(TIM,    <<"31a07b20-c5fe-11e5-9912-ba0be0483c18">>).

-define(ALICE_JID, jid:make(?ALICE, ?LOCAL_CONTEXT, <<>>)).
-define(BOB_JID,   jid:make(?BOB,   ?LOCAL_CONTEXT, <<>>)).
-define(CAROL_JID, jid:make(?CAROL, ?LOCAL_CONTEXT, <<>>)).
-define(KAREN_JID, jid:make(?KAREN, ?LOCAL_CONTEXT, <<>>)).
-define(ROBERT_JID,jid:make(?ROBERT,?LOCAL_CONTEXT, <<>>)).
-define(TIM_JID,   jid:make(?TIM,   ?LOCAL_CONTEXT, <<>>)).
-define(SERVER_JID,jid:make(<<>>,   ?LOCAL_CONTEXT, <<>>)).

-define(ALICE_B_JID,  jid:to_binary(?ALICE_JID)).
-define(BOB_B_JID,    jid:to_binary(?BOB_JID)).
-define(CAROL_B_JID,  jid:to_binary(?CAROL_JID)).
-define(KAREN_B_JID,  jid:to_binary(?KAREN_JID)).
-define(ROBERT_B_JID, jid:to_binary(?ROBERT_JID)).
-define(TIM_B_JID,    jid:to_binary(?TIM_JID)).
-define(SERVER_B_JID, jid:to_binary(?SERVER_JID)).

-define(AVATAR_FILE,   <<"ee880476-e712-11e5-990d-08002719e96e">>).
-define(AVATAR_FILE2,  <<"5620c844-e98d-11e5-b97b-08002719e96e">>).
-define(AVATAR_FILE3,  <<"d19434ee-ea5e-11e5-b413-08002719e96e">>).
-define(MEDIA_FILE,    <<"ff9451f2-e712-11e5-9ab0-08002719e96e">>).
-define(GC_MEDIA_FILE, <<"5f4dab84-2d18-11e6-b1e2-73907483f182">>).

-define(AVATAR_CHUNK,   <<"24634ec0-e96b-11e5-8f6e-08002719e96e">>).
-define(AVATAR_CHUNK2,  <<"54559742-e98d-11e5-bc3d-08002719e96e">>).
-define(AVATAR_CHUNK3,  <<"d19434ee-ea5e-11e5-b413-08002719e96e">>).
-define(MEDIA_CHUNK,    <<"24f99f56-e96b-11e5-8e11-08002719e96e">>).
-define(GC_MEDIA_CHUNK, <<"5fd2b928-2d18-11e6-bcb5-e777f9044f41">>).

-define(AVATAR_DATA,  <<"avatar file contents">>).
-define(AVATAR_DATA2, <<"alternative avatar file contents">>).
-define(AVATAR_DATA3, <<"bob's avatar file contents">>).
-define(MEDIA_DATA,   <<"media file contents">>).

-define(FILENAME,     <<"photo of cat.jpg">>).
-define(URL_FILENAME, <<"photo%20of%20cat.jpg">>).

-define(PRIVACY_LIST1, <<"privacy list 1">>).
-define(PRIVACY_LIST2, <<"privacy list 2">>).

-define(PRIVACY_ITEM1, <<"3f2d5e24-ecba-11e5-951e-08002719e96e">>).
-define(PRIVACY_ITEM2, <<"3fa76372-ecba-11e5-a383-08002719e96e">>).
-define(PRIVACY_ITEM3, <<"3fd96ee4-ecba-11e5-83d6-08002719e96e">>).

-define(GROUP_CHAT,  <<"e2570260-2c7c-11e6-8bbd-1364d78a5dd4">>).
-define(GROUP_CHAT2, <<"832a611e-7f9d-11e6-aa1a-0800278fd327">>).
-define(GROUP_CHAT_JID, jid:make(?GROUP_CHAT, ?LOCAL_CONTEXT, <<>>)).
-define(GROUP_CHAT2_JID, jid:make(?GROUP_CHAT2, ?LOCAL_CONTEXT, <<>>)).

-define(CHAT_TITLE, <<"Chatty McChatface">>).

-define(BOT, <<"e2570262-1c7c-10e6-8bbd-1362d78a5dd4">>).
-define(BOT_B_JID, <<?LOCAL_CONTEXT/binary, "/bot/", ?BOT/binary>>).
-define(BOT_JID, jid:from_binary(?BOT_B_JID)).
-define(BOT_TITLE, <<"Alice's Bot">>).
-define(BOT_NAME, <<"AliceBot">>).
-define(BOT_DESC, <<"A test bot owned by Alice">>).
-define(BOT_ADDRESS, <<"260 Tinakori Road, Thorndon, Wellington">>).
-define(BOT_TYPE, <<"LucyLiuBot">>).
-define(BOT_LAT, 55.0).
-define(BOT_LON, 60.1).
-define(BOT_TAGS, [<<"bot_tag_1">>, <<"bot_tag_2">>]).
-define(BOT_RADIUS, 10000).
-define(BOT_OWNER_ROSTER, [?BOB_B_JID, ?KAREN_B_JID]).
-define(BOT_OWNER_ROSTER_VERSION, <<"999-4">>).
-define(BOT_UPDATE_STANZA, <<"<bot_update>A thing happened</bot_update>">>).

-define(ROSTER_VIEWERS, [?BOB_B_JID]).

-define(ITEM, <<"test-item-id">>).
-define(ITEM2, <<"test-item-id2">>).
-define(ITEM_PUB_TIME, 8888).
-define(ITEM_UPDATE_TIME, 9999).
-define(ITEM_IMAGE, <<"Some image or other">>).
-define(ITEM_STANZA,
        <<"<entry xmlns='", ?NS_ATOM/binary, "'>",
          "<content>This is a note I wrote on a boat in a moat</content>",
          "<image>", ?ITEM_IMAGE/binary, "</image>",
          "</entry>">>).
-define(ITEM_STANZA2,
        <<"<entry xmlns='", ?NS_ATOM/binary, "'>",
          "<content>'Does it float?' he wrote, 'I hope not(e)'.</content>",
          "</entry>">>).

-define(HS_V_1, <<"683b9208-a09d-11e6-adaa-583a00000762">>).
-define(HS_V_2, <<"6a39d1be-a09d-11e6-adaa-583a00000762">>).
-define(HS_V_3, <<"7099d8c4-a09d-11e6-adaa-583a00000762">>).

-define(BOB_HS_ITEM_COUNT, 250).

-endif.
