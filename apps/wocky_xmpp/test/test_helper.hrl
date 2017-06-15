-ifndef(TEST_HELPER_HRL).
-define(TEST_HELPER_HRL, true).

-include("wocky.hrl").

-define(ALICE,  <<"043e8c96-ba30-11e5-9912-ba0be0483c18">>).
-define(CAROL,  <<"1e6f3b36-c5fe-11e5-9912-ba0be0483c18">>).
-define(BOB,    <<"2396aa72-c5fe-11e5-9912-ba0be0483c18">>).
-define(KAREN,  <<"2841ef1e-c5fe-11e5-9912-ba0be0483c18">>).
-define(ROBERT, <<"2c33f32e-c5fe-11e5-9912-ba0be0483c18">>).
-define(TIM,    <<"31a07b20-c5fe-11e5-9912-ba0be0483c18">>).

-define(SERVER,      <<"localhost">>).

-define(JID(User), jid:make(User, ?SERVER, <<>>)).
-define(BJID(User), jid:to_binary(?JID(User))).

-define(PHONE_NUMBER,<<"+1234">>).
-define(EXTERNAL_ID, <<"701990807448920064">>).

-define(AVATAR_FILE,   <<"ee880476-e712-11e5-990d-08002719e96e">>).

-define(BOT, <<"e2570262-1c7c-10e6-8bbd-1362d78a5dd4">>).
-define(BOT_B_JID, <<?SERVER/binary, "/bot/", ?BOT/binary>>).
-define(BOT_JID, jid:from_binary(?BOT_B_JID)).

-define(ITEM, <<"test-item-id">>).
-define(ITEM2, <<"test-item-id2">>).
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

-record(item, {id, version, from, stanzas}).

% Reproduced from escalus.hrl. We can't include it directly because it
% redefines the `jid` record :(
-record(client, {
        jid :: binary() | undefined,
        module :: atom(),
        socket :: term(),
        ssl :: boolean(),
        compress :: {zlib, {Zin::zlib:zstream(), Zout::zlib:zstream()}}
                 |  false,
        rcv_pid :: pid(),
        event_client :: any()
       }).

-endif. % ifdef TEST_HELPER_HRL
