%%% @copyright 2016+ Hippware, Inc.
%%% @doc Test suite for tros_permissions.erl
-module(tros_permissions_tests).


-include_lib("eunit/include/eunit.hrl").
-include("wocky_db_seed.hrl").

-import(tros_permissions, [can_upload/3, can_download/3]).

tros_permissions_test_() -> {
  "tros_permissions",
  setup, fun before_all/0, fun after_all/1,
  [ {inparallel, [
      test_can_upload(),
      test_can_download()
  ]}]
}.

before_all() ->
    ok = wocky_db:prepare_tables(shared, [roster, user]),
    ok = wocky_db:prepare_tables(?LOCAL_CONTEXT, [group_chat]),
    {ok, _} = wocky_db_seed:seed_tables(shared, [roster, user]),
    ok = wocky_db_seed:seed_tables(?LOCAL_CONTEXT, [group_chat]),
    ok.

after_all(_) ->
    ok.

test_can_upload() ->
    Access = <<"members:", (jid:to_binary(?GROUP_CHAT_JID))/binary>>,
    { "can_upload", [
      { "avatars are always able to be uploaded", [
        ?_assert(can_upload(?ALICE_JID, <<"avatar">>, <<>>)),
        ?_assert(can_upload(?BOB_JID, <<"avatar">>, <<>>)),
        ?_assert(can_upload(?TIM_JID, <<"avatar">>, <<>>))
      ]},
      { "message media can always be uploaded", [
        ?_assert(can_upload(?ALICE_JID, <<"message_media">>, <<>>)),
        ?_assert(can_upload(?BOB_JID, <<"message_media">>, <<>>)),
        ?_assert(can_upload(?TIM_JID, <<"message_media">>, <<>>))
      ]},
      { "group chat media can only be uploaded by chat participants", [
        ?_assert(can_upload(?ALICE_JID, <<"group_chat_media">>, Access)),
        ?_assert(can_upload(?BOB_JID, <<"group_chat_media">>, Access)),
        ?_assertNot(can_upload(?TIM_JID, <<"group_chat_media">>, Access))
      ]},
      { "no other types can be uploaded", [
        ?_assertNot(can_upload(?ALICE_JID, <<"fnord">>, <<>>)),
        ?_assertNot(can_upload(?ALICE_JID, <<"blah">>, <<>>))
      ]}
    ]}.

test_can_download() ->
    Owner = ?ALICE,
    Access = <<"user:", (?BOB_B_JID)/binary,
               ",members:", (jid:to_binary(?GROUP_CHAT2_JID))/binary>>,
    FriendsAccess = <<"friends:", (?ALICE_B_JID)/binary>>,
    AllAccess = <<"all">>,
    { "can_download", [
      { "owner can always download", [
        ?_assert(can_download(?ALICE_JID, Owner, Access))
      ]},
      { "specified user can download", [
        ?_assert(can_download(?BOB_JID, Owner, Access))
      ]},
      { "specified group chat members can download", [
        ?_assert(can_download(?TIM_JID, Owner, Access)),
        ?_assert(can_download(?KAREN_JID, Owner, Access))
      ]},
      { "nobody else can download", [
        ?_assertEqual({false, permission_denied},
                      can_download(?ROBERT_JID, Owner, Access)),
        ?_assertEqual({false, permission_denied},
                      can_download(?CAROL_JID, Owner, Access))
      ]},
      { "only friends of the specified user can download", [
        ?_assert(can_download(?ALICE_JID, Owner, FriendsAccess)),
        ?_assert(can_download(?BOB_JID, Owner, FriendsAccess)),
        ?_assert(can_download(?CAROL_JID, Owner, FriendsAccess)),
        ?_assert(can_download(?ROBERT_JID, Owner, FriendsAccess)),
        ?_assert(can_download(?KAREN_JID, Owner, FriendsAccess)),
        ?_assertEqual({false, permission_denied},
                      can_download(?TIM_JID, Owner, FriendsAccess))
      ]},
      { "only friends of the specified user can download", [
        ?_assert(can_download(?ALICE_JID, Owner, AllAccess)),
        ?_assert(can_download(?BOB_JID, Owner, AllAccess)),
        ?_assert(can_download(?CAROL_JID, Owner, AllAccess)),
        ?_assert(can_download(?ROBERT_JID, Owner, AllAccess)),
        ?_assert(can_download(?KAREN_JID, Owner, AllAccess)),
        ?_assert(can_download(?TIM_JID, Owner, AllAccess))
      ]}
    ]}.

