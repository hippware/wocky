%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite mod_conversations.erl
-module(conversation_SUITE).
-compile(export_all).
-compile({parse_transform, fun_chain}).
-compile({parse_transform, cut}).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("wocky.hrl").
-include("wocky_db_seed.hrl").

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() -> [
          all_conversations,
          first_2_conversations,
          last_2_conversations,
          past_end,
          before_beginning,
          middle_2,
          all_no_set
         ].

suite() ->
    escalus:suite().


%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ok = test_helper:ensure_wocky_is_running(),
    fun_chain:first(Config,
        escalus:init_per_suite(),
        test_helper:setup_users([alice, bob, carol, karen, robert]),
        seed_conversations()
    ).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

all_conversations(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        Stanza = query_stanza(undefined, undefined, undefined, undefined),
        Result = test_helper:expect_iq_success_u(Stanza, Alice),
        check_ret(Result, 4, 4, 0)
      end).

first_2_conversations(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        Stanza = query_stanza(2, undefined, undefined, undefined),
        Result = test_helper:expect_iq_success_u(Stanza, Alice),
        check_ret(Result, 2, 4, 0)
      end).

last_2_conversations(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        Stanza = query_stanza(undefined, 2, undefined, undefined),
        Result = test_helper:expect_iq_success_u(Stanza, Alice),
        Items = check_ret(Result, 2, 4, 2),

        Stanza2 = query_stanza(2, undefined, undefined, before),
        Result2 = test_helper:expect_iq_success_u(Stanza2, Alice),
        Items2 = check_ret(Result2, 2, 4, 2),

        ?assertEqual(Items, Items2)
      end).

past_end(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        Stanza = query_stanza(undefined, 4, undefined, undefined),
        Result = test_helper:expect_iq_success_u(Stanza, Alice),
        check_ret(Result, 0, 4, undefined)
      end).

before_beginning(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        Stanza = query_stanza(1, undefined, undefined, undefined),
        Result = test_helper:expect_iq_success_u(Stanza, Alice),
        [Item] = check_ret(Result, 1, 4, 0),

        Stanza2 = query_stanza(undefined, undefined, get_id(Item), before),
        Result2 = test_helper:expect_iq_success_u(Stanza2, Alice),
        check_ret(Result2, 0, 4, undefined)
      end).

middle_2(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        Stanza = query_stanza(undefined, undefined, undefined, undefined),
        Result = test_helper:expect_iq_success_u(Stanza, Alice),
        [First, _, _, Last] = check_ret(Result, 4, 4, 0),

        Stanza2 = query_stanza(2, undefined, get_id(First), 'after'),
        Result2 = test_helper:expect_iq_success_u(Stanza2, Alice),
        Items = check_ret(Result2, 2, 4, 1),

        Stanza3 = query_stanza(2, undefined, get_id(Last), before),
        Result3 = test_helper:expect_iq_success_u(Stanza3, Alice),
        ?assertEqual(Items, check_ret(Result3, 2, 4, 1))
      end).

all_no_set(Config) ->
    escalus:story(Config, [{alice, 1}], fun(Alice) ->
        Stanza = query_stanza([]),
        Result = test_helper:expect_iq_success_u(Stanza, Alice),
        check_ret(Result, 4, 4, 0)
      end).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

query_stanza(Children) ->
    test_helper:iq_set(?NS_CONVERSATIONS,
                       #xmlel{name = <<"query">>,
                              children = Children}).

query_stanza(MaxCount, StartIndex, StartID, Direction) ->
    Set = #xmlel{name = <<"set">>,
                 attrs = [{<<"xmlns">>, ?NS_RSM}],
                 children = defined([max(MaxCount), index(StartIndex),
                                     id(StartID, Direction)])},
    query_stanza([Set]).

defined(L) ->
    [E || E <- L, E =/= undefined].

max(undefined) -> undefined;
max(Count) -> el(<<"max">>, integer_to_binary(Count)).

id(undefined, undefined) -> undefined;
id(undefined, before) -> #xmlel{name = <<"before">>};
id(ID, undefined) -> id(ID, 'after');
id(ID, Direction) -> el(atom_to_binary(Direction, utf8), ID).

index(undefined) -> undefined;
index(Index) -> el(<<"index">>, integer_to_binary(Index)).

el(Name, Content) ->
    #xmlel{name = Name, children = [#xmlcdata{content = Content}]}.

check_ret(#xmlel{name = <<"iq">>,
                 children = [#xmlel{name = <<"query">>,
                                    attrs = [{<<"xmlns">>, ?NS_CONVERSATIONS}],
                                    children = Children}]},
          ItemCount, Count, Index) ->
    Items = check_children(Children, Count, Index, 0, [], []),
    ?assertEqual(ItemCount, length(Items)),
    lists:reverse(Items).

check_children([], _Count, _Index, _LastID, _SeenOthers, Acc) ->
    Acc;
check_children([I = #xmlel{name = <<"item">>} | Rest], Count, Index,
               LastID, SeenOthers, Acc) ->
    OtherJID = get_other_jid(I),
    ?assertNot(lists:member(OtherJID, SeenOthers)),
    ID = binary_to_integer(get_id(I)),
    ?assert(ID > LastID),
    check_children(Rest, Count, Index, ID, [OtherJID | SeenOthers], [I | Acc]);
check_children([Set = #xmlel{name = <<"set">>,
                             attrs = [{<<"xmlns">>, ?NS_RSM}]
                            } | Rest],
               Count, Index, LastID, SeenOthers, Acc) ->
    check_count(Set, Count),
    check_index(Set, Index),
    check_children(Rest, Count, Index, LastID, SeenOthers, Acc).

check_count(Set, Count) ->
    Val = xml:get_path_s(Set, [{elem, <<"count">>}, cdata]),
    ?assertEqual(Count, binary_to_integer(Val)).

check_index(Set, undefined) ->
    ?assertEqual(<<>>, get_first_index(Set));
check_index(Set, Index) ->
    ?assertEqual(Index, binary_to_integer(get_first_index(Set))).

get_first_index(Set) ->
    xml:get_path_s(Set, [{elem, <<"first">>}, {attr, <<"index">>}]).

get_other_jid(Item) ->
    xml:get_path_s(Item, [{elem, <<"other_jid">>}, cdata]).

get_id(Item) ->
    xml:get_path_s(Item, [{attr, <<"id">>}]).

%%--------------------------------------------------------------------
%% Data seeding
%%--------------------------------------------------------------------

seed_conversations(Config) ->
    ?wocky_repo:delete_all(?wocky_conversation),
    Convos = random_conversation_list(),
    lists:foreach(archive_message(_), Convos),
    Config.

archive_message(#{id := ID,
                  user_jid := UserJID,
                  other_jid := OtherJID,
                  outgoing := Outgoing,
                  message := XML}) ->
    mod_wocky_conversations:archive_message_hook(
      ok,
      ?LOCAL_CONTEXT,
      ID,
      unused,
      jid:from_binary(UserJID),
      jid:from_binary(OtherJID),
      unused,
      direction(Outgoing),
      element(2, exml:parse(XML))).

direction(true) -> outgoing;
direction(false) -> incoming.

archive_users() ->
    [jid:to_binary(J) ||
     J <- [
           ?ALICE_JID,
           ?BOB_JID,
           ?CAROL_JID,
           ?KAREN_JID,
           ?ROBERT_JID
          ]].

random_conversation_list() ->
    Messages = sort_by_time(random_message_history()),
    unique_pairs(Messages, [], []).

unique_pairs([], _, Acc) -> Acc;
unique_pairs([M = #{user_jid := U, other_jid := O} | Rest], Pairs, Acc) ->
    Pair = {U, O},
    case lists:member(Pair, Pairs) of
        true -> unique_pairs(Rest, Pairs, Acc);
        false -> unique_pairs(Rest, [Pair | Pairs], [M | Acc])
    end.

random_message_history() ->
    _ = rand:seed(exsplus, {1, 2, 3}),
    [random_message(ID, archive_users()) || ID <- lists:seq(1, 300)].

random_message(ID, Users) ->
    From = lists:nth(rand:uniform(length(Users)), Users),
    RemainingUsers = Users -- [From],
    To = lists:nth(rand:uniform(length(RemainingUsers)), RemainingUsers),
    archive_row(ID, From, To).

archive_row(ID, From, To) ->
    #{user_jid => From,
      other_jid => To,
      id => ID,
      time => ID,
      outgoing => rand:uniform(2) =:= 1,
      message => msg_xml_packet(<<To/binary,
                                  (integer_to_binary(ID))/binary>>)
     }.

sort_by_time(ArchiveRows) ->
    lists:sort(fun sort_by_time/2, ArchiveRows).

% Sorts newest first
sort_by_time(#{time := T1}, #{time := T2}) ->
    T1 > T2.

msg_xml_packet(Handle) ->
    <<"<message xml:lang=\"en\" type=\"chat\"
                to=\"278e4ba0-b9ae-11e5-a436-080027f70e96@localhost\">
           <body>Hi, ", Handle/binary, "</body>
      </message>">>.
