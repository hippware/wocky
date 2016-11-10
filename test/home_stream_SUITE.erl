%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite for home stream
-module(home_stream_SUITE).
-compile(export_all).
-compile({parse_transform, fun_chain}).

-include("wocky.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("wocky_db_seed.hrl").

-compile({parse_transform, cut}).
-compile({parse_transform, do}).

-import(test_helper, [check_attr/3, expect_iq_success_u/3, expect_iq_error_u/3,
                      rsm_elem/1, add_to_u/2, ensure_all_clean/1]).

-record(item, {id, version, from, stanza}).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() -> [
          get,
          publish,
          delete,
          auto_publish,
          subscribe,
          subscribe_version,
          unsubscribe,
          get_item
         ].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

users() ->
    [alice, bob].

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),
    wocky_db:clear_user_tables(?LOCAL_CONTEXT),
    wocky_db_seed:seed_tables(?LOCAL_CONTEXT, [home_stream]),
    Users = escalus:get_users(users()),
    fun_chain:first(Config,
        escalus:init_per_suite(),
        escalus:create_users(Users)
    ).

end_per_suite(Config) ->
    escalus:delete_users(Config, escalus:get_users(users())),
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Test suites
%%--------------------------------------------------------------------

get(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        Stanza = expect_iq_success_u(get_stanza(), Alice, Alice),
        check_result(Stanza, 3),

        % Bob can't see Alice's home stream
        expect_iq_error_u(get_stanza(), Bob, Alice),

        % Bob's own stream is empty
        Stanza2 = expect_iq_success_u(get_stanza(), Bob, Bob),
        check_result(Stanza2, 0, 0, false)
      end).

publish(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        % Server can also auto-generate IDs if none is defined:
        expect_iq_success_u(pub_stanza(undefined), Alice, Alice),

        expect_iq_success_u(pub_stanza(<<"some_id">>), Alice, Alice),

        % Bob can't publish to Alice's home stream
        expect_iq_error_u(pub_stanza(<<"other_id">>), Bob, Alice),

        Stanza = expect_iq_success_u(get_stanza(), Alice, Alice),
        check_result(Stanza, 5)
      end).

delete(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        Stanza = expect_iq_success_u(get_stanza(), Alice, Alice),
        check_result(Stanza, 5),

        expect_iq_success_u(delete_stanza(), Alice, Alice),

        % Bob can't delete from Alice's home stream
        expect_iq_error_u(delete_stanza(), Bob, Alice),

        Stanza2 = expect_iq_success_u(get_stanza(), Alice, Alice),
        Items = check_result(Stanza2, 4, 1, true),
        ?assertEqual({delete, <<"some_id">>}, lists:last(Items))
      end).

auto_publish(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        Stanza = escalus_stanza:chat_to(?ALICE_B_JID, <<"All your base">>),
        escalus:send(Bob, Stanza),
        escalus:assert(is_chat_message, escalus_client:wait_for_stanza(Alice)),

        Stanza2 = escalus_stanza:chat_to(?ALICE_B_JID, <<"All your base2">>),
        escalus:send(Bob, Stanza2),
        escalus:assert(is_chat_message, escalus_client:wait_for_stanza(Alice)),

        %% There should only be 5 items because the latter chat overwrites the
        %% former
        Stanza3 = expect_iq_success_u(get_stanza(), Alice, Alice),
        Items = check_result(Stanza3, 5, 1, true),
        escalus:assert(is_chat_message, hd((lists:last(Items))#item.stanza)),

        Stanza4 = bot_share_msg(?ALICE_B_JID, <<"msg">>, wocky_db:create_id()),
        escalus:send(Bob, Stanza4),
        escalus:assert(is_chat_message, escalus_client:wait_for_stanza(Alice)),

        %% There should now be 6 items because the bot share messaege won't
        %% overwrite any others
        Stanza5 = expect_iq_success_u(get_stanza(), Alice, Alice),
        Items2 = check_result(Stanza5, 6, 1, true),
        escalus:assert(is_chat_message, hd((lists:last(Items2))#item.stanza)),

        Stanza6 = bot_share_msg(?ALICE_B_JID, <<"msg">>, wocky_db:create_id()),
        escalus:send(Bob, Stanza6),
        escalus:assert(is_chat_message, escalus_client:wait_for_stanza(Alice)),

        Stanza7 = expect_iq_success_u(get_stanza(), Alice, Alice),
        Items3 = check_result(Stanza7, 7, 1, true),
        escalus:assert(is_chat_message, hd((lists:last(Items3))#item.stanza))
      end).

subscribe(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        escalus:send(Alice,
            escalus_stanza:presence_direct(hs_node(?ALICE), <<"available">>,
                                           query_el(undefined))),

        escalus:send(Alice,
                     add_to_u(pub_stanza(<<"new_item">>), Alice)),
        escalus:assert_many([is_iq_result, is_message],
                            escalus:wait_for_stanzas(Alice, 2)),

        Stanza = escalus_stanza:chat_to(?ALICE_B_JID, <<"All your base">>),
        escalus:send(Bob, Stanza),

        escalus:assert_many([is_message, is_message],
                            escalus:wait_for_stanzas(Alice, 2)),

        timer:sleep(500),
        ensure_all_clean([Alice, Bob])
      end).

subscribe_version(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
        escalus:send(Alice,
            escalus_stanza:presence_direct(hs_node(?ALICE), <<"available">>,
                                           query_el(?HS_V_2))),

        lists:foreach(
          fun(_) ->
                  escalus:assert(is_message, escalus:wait_for_stanza(Alice))
          end, lists:seq(1, 7)),

        %% Bob should get nothing from his own HS (since it's empty) nor from
        %% Alice's (since it's not his)
        escalus:send(Bob,
            escalus_stanza:presence_direct(hs_node(?BOB), <<"available">>,
                                           query_el(?HS_V_2))),
        escalus:send(Bob,
            escalus_stanza:presence_direct(hs_node(?ALICE), <<"available">>,
                                           query_el(?HS_V_2))),

        %% The presence we send to alice's home stream will be rejected
        escalus:assert(is_presence_error(_), escalus:wait_for_stanza(Bob)),

        escalus:send(Alice,
                     add_to_u(pub_stanza(<<"new_item2">>), Alice)),
        escalus:assert_many([is_iq_result, is_message],
                            escalus:wait_for_stanzas(Alice, 2)),
        timer:sleep(500),

        ensure_all_clean([Alice, Bob])
      end).

unsubscribe(Config) ->
    escalus:story(Config, [{alice, 1}],
      fun(Alice) ->
        escalus:send(Alice,
            escalus_stanza:presence_direct(hs_node(?ALICE), <<"available">>,
                                           query_el(undefined))),

        escalus:send(Alice,
            escalus_stanza:presence_direct(hs_node(?ALICE), <<"unavailable">>,
                                           query_el(undefined))),

        expect_iq_success_u(pub_stanza(<<"new_item3">>), Alice, Alice),
        timer:sleep(500),

        ensure_all_clean([Alice])
      end).

get_item(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}],
      fun(Alice, _Bob) ->
        Stanza = expect_iq_success_u(get_stanza(?ITEM), Alice, Alice),
        check_single_result(Stanza, ?ITEM),

        % Deleted and never-existed items should both return not-found
        expect_iq_error_u(get_stanza(<<"some_id">>), Alice, Alice),
        expect_iq_error_u(get_stanza(wocky_db:create_id()), Alice, Alice)
      end).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

get_stanza() ->
    test_helper:iq_get(?NS_PUBLISHING,
                       #xmlel{name = <<"items">>,
                              attrs = [{<<"node">>, ?HOME_STREAM_NODE}],
                              children = [rsm_elem(#rsm_in{max = 200})]}).

get_stanza(ID) ->
    test_helper:iq_get(?NS_PUBLISHING,
                       #xmlel{name = <<"items">>,
                              attrs = [{<<"node">>, ?HOME_STREAM_NODE}],
                              children = [#xmlel{name = <<"item">>,
                                                 attrs = [{<<"id">>, ID}]}]}).

pub_stanza(ID) ->
    test_helper:iq_set(?NS_PUBLISHING,
                       #xmlel{name = <<"publish">>,
                              attrs = [{<<"node">>, ?HOME_STREAM_NODE}],
                              children = [new_item(ID)]}).

delete_stanza() ->
    test_helper:iq_set(?NS_PUBLISHING,
                       #xmlel{name = <<"publish">>,
                              attrs = [{<<"node">>, ?HOME_STREAM_NODE}],
                              children = [delete_item()]}).

new_item(ID) ->
    #xmlel{name = <<"item">>,
           attrs = maybe_id(ID),
           children = [#xmlel{name = <<"new-published-element">>,
                              children = [#xmlcdata{content =
                                                    <<"hello there!">>}]},
                       #xmlel{name = <<"second-element">>,
                              attrs = [{<<"test_attr">>, <<"abc">>}]}]}.

maybe_id(undefined) -> [];
maybe_id(ID) -> [{<<"id">>, ID}].

delete_item() ->
    #xmlel{name = <<"delete">>,
           attrs = [{<<"id">>, <<"some_id">>}]}.

check_result(Stanza, NumItems) ->
    check_result(Stanza, NumItems, 0, true).
check_result(Stanza, NumItems, NumDeletes, CheckVersion) ->
    {ok, L} = do([error_m ||
                  ItemsEl <- get_items_el(Stanza),
                  check_attr(<<"node">>, ?HOME_STREAM_NODE, ItemsEl),
                  check_attr(<<"xmlns">>, ?NS_PUBLISHING, ItemsEl),
                  maybe(CheckVersion, check_attr(<<"version">>, any, ItemsEl)),
                  ItemList <- get_items(ItemsEl),
                  check_elements(ItemsEl, NumItems, NumDeletes, 1),
                  {ok, ItemList}
                 ]),
    L.

check_single_result(Stanza, ID) ->
    {ok, L} = do([error_m ||
                  ItemsEl <- get_items_el(Stanza),
                  check_attr(<<"node">>, ?HOME_STREAM_NODE, ItemsEl),
                  check_attr(<<"xmlns">>, ?NS_PUBLISHING, ItemsEl),
                  ItemList <- get_items(ItemsEl),
                  check_elements(ItemsEl, 1, 0, 0),
                  {ok, ?assertEqual(ID, (hd(ItemList))#item.id)},
                  {ok, hd(ItemList)}
                 ]),
    L.

get_items_el(Stanza) ->
    case xml:get_subtag(Stanza, <<"items">>) of
        false -> {error, no_items};
        El -> {ok, El}
    end.

get_items(Stanza) ->
    {ok, lists:reverse(lists:foldl(get_item(_, _), [], Stanza#xmlel.children))}.

get_item(#xmlel{name = <<"item">>, attrs = Attrs, children = Children}, Acc) ->
    {value, ID} = xml:get_attr(<<"id">>, Attrs),
    {value, Version} = xml:get_attr(<<"version">>, Attrs),
    {value, From} = xml:get_attr(<<"from">>, Attrs),
    [#item{id = ID,
           version = Version,
           from = From,
           stanza = Children}
     | Acc];
get_item(#xmlel{name = <<"delete">>, attrs = Attrs, children = []}, Acc) ->
    {value, ID} = xml:get_attr(<<"id">>, Attrs),
    [{delete, ID} | Acc];
get_item(_, Acc) ->
    Acc.

check_elements(Items, NumItems, NumDeletes, NumRSM) ->
    ?assertEqual(NumItems, count_elements(Items, <<"item">>)),
    ?assertEqual(NumDeletes, count_elements(Items, <<"delete">>)),
    ?assertEqual(NumRSM, count_elements(Items, <<"set">>)),
    ok.

count_elements(#xmlel{name = <<"items">>, children = Children}, Type) ->
    length(
      lists:filter(fun(#xmlel{name = T}) when T =:= Type -> true;
                      (_) -> false
                   end,
                   Children)).

query_el(Version) ->
    #xmlel{name = <<"query">>,
           attrs = [{<<"xmlns">>, ?NS_PUBLISHING} |
                    maybe_version_attr(Version)]}.

maybe_version_attr(undefined) -> [];
maybe_version_attr(Version) -> [{<<"version">>, Version}].

hs_node(User) ->
    jid:to_binary(
      jid:make(User, ?LOCAL_CONTEXT, <<"home_stream">>)).

is_presence_error(Stanza) ->
    escalus_pred:is_presence(Stanza)
    andalso
    escalus_pred:is_error(<<"cancel">>, <<"not-acceptable">>, Stanza).

maybe(false, _) -> ok;
maybe(true, X) -> X.

bot_share_msg(Recipient, Msg, BotID) ->
    BaseMsg = escalus_stanza:chat_to(Recipient, Msg),
    BaseMsg#xmlel{children = [#xmlel{name = <<"bot">>,
                                     children = [jid_el(BotID) |
                                                 BaseMsg#xmlel.children]}]}.

jid_el(BotID) ->
    #xmlel{name = <<"jid">>,
           children = [#xmlcdata{content = BotID}]}.
