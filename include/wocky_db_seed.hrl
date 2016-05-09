%%% @copyright 2016+ Hippware, Inc.
%%% @doc Macros useful for seed data used in tests

-ifndef(WOCKY_DB_SEED_HRL).
-define(WOCKY_DB_SEED_HRL, 1).

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
-define(SERVER_JID,jid:make(<<>>,   ?LOCAL_CONTEXT, <<>>)).

-define(AVATAR_FILE,  <<"ee880476-e712-11e5-990d-08002719e96e">>).
-define(AVATAR_FILE2, <<"5620c844-e98d-11e5-b97b-08002719e96e">>).
-define(AVATAR_FILE3, <<"d19434ee-ea5e-11e5-b413-08002719e96e">>).
-define(MEDIA_FILE,   <<"ff9451f2-e712-11e5-9ab0-08002719e96e">>).

-define(AVATAR_CHUNK,  <<"24634ec0-e96b-11e5-8f6e-08002719e96e">>).
-define(AVATAR_CHUNK2, <<"54559742-e98d-11e5-bc3d-08002719e96e">>).
-define(AVATAR_CHUNK3, <<"d19434ee-ea5e-11e5-b413-08002719e96e">>).
-define(MEDIA_CHUNK,   <<"24f99f56-e96b-11e5-8e11-08002719e96e">>).

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

-define(CHAT_TITLE, <<"Chatty McChatface">>).
-endif.
