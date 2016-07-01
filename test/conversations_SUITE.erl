%%% @copyright 2016+ Hippware, Inc.
%%% @doc Integration test suite mod_conversations.erl
-module(conversations_SUITE).
-compile(export_all).
-compile({parse_transform, fun_chain}).

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
    wocky_db_seed:clear_user_tables(?LOCAL_CONTEXT),
    wocky_db_seed:seed_table(?LOCAL_CONTEXT, conversation),
    Users = escalus:get_users([alice]),
    fun_chain:first(Config,
        escalus:init_per_suite(),
        escalus:create_users(Users)
    ).

end_per_suite(Config) ->
    escalus:delete_users(Config, escalus:get_users([alice])),
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
    Query = #xmlel{name = <<"query">>,
                   attrs = [{<<"xmlns">>, ?NS_CONVERSATIONS}],
                   children = Children},
    #xmlel{name = <<"iq">>,
           attrs = [{<<"type">>, <<"set">>},
                    {<<"id">>, integer_to_binary(rand:uniform(10000))}],
           children = [Query]}.

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
    Items = check_children(Children, Count, Index, 1 bsl 64, [], []),
    ?assertEqual(ItemCount, length(Items)),
    lists:reverse(Items).

check_children([], _Count, _Index, _LastID, _SeenOthers, Acc) ->
    Acc;
check_children([I = #xmlel{name = <<"item">>} | Rest], Count, Index,
               LastID, SeenOthers, Acc) ->
    OtherJID = get_other_jid(I),
    ?assertNot(lists:member(OtherJID, SeenOthers)),
    ID = binary_to_integer(get_id(I)),
    ?assert(ID < LastID),
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
