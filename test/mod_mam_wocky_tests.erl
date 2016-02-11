%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for mod_mam_wocky.erl
-module(mod_mam_wocky_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd_config.hrl").
-include("wocky_db_seed.hrl").

-import(mod_mam_wocky, [
                        archive_message/9,
                        lookup_messages/14]).

-define(FIRST_ID(N), nth_id(U1, U2, N, Rows)).

mod_mam_wocky_test_() -> {
  "mod_mam_wocky",
  setup, fun before_all/0, fun after_all/1,
  [
   test_archive_message(),
   test_lookup_messages()
  ]
 }.

before_all() ->
    meck:new(gen_mod, [passthrough]),
    meck:expect(gen_mod, get_module_opt,
                fun(global, mod_mam_wocky, message_ttl, infinity) -> infinity
                end),
    ets:new(config, [named_table, set, public, {keypos, 2}]),
    ets:insert(config, #config{key = hosts, value = [<<"localhost">>]}),
    application:ensure_started(p1_stringprep),
    ok = wocky_app:start(),
    ok = wocky_db_seed:prepare_tables(?LOCAL_CONTEXT, [message_archive]),
    ok.

after_all(_) ->
    ets:delete(config),
    meck:validate(gen_mod),
    meck:unload(gen_mod),
    ok = wocky_app:stop().

before_each() ->
    ok = wocky_db_seed:clear_tables(?LOCAL_CONTEXT, [message_archive]),
    {ok, Rows} = wocky_db_seed:seed_table(?LOCAL_CONTEXT, message_archive),
    Rows.

after_each(_) ->
    ok = wocky_db_seed:clear_tables(?LOCAL_CONTEXT, [message_archive]).

test_archive_message() ->
    Users = wocky_db_seed:archive_users(),
    U1 = hd(Users),
    U2 = lists:last(Users),

    OrigMsg = wocky_db_seed:msg_xml_packet(<<"Test Handle">>),
    {ok, MsgXML} = exml:parse(OrigMsg),
    MsgBin = exml:to_binary(MsgXML),
    ID = 888888,
    Lower = min(U1, U2),
    Upper = max(U1, U2),
    Row = #{id => ID, lower_jid => Lower, upper_jid => Upper,
            sent_to_lower => U1 < U2, message => MsgBin},
    SelectRow = #{lower_jid => Lower, upper_jid => Upper},

    { "archive_message", [
        { "Message should archive safely and should only be stored once", [
            ?_test(
               begin
                   [?assertEqual(ok, archive_message(ok, ?LOCAL_CONTEXT, ID,
                                                     not_used,
                                                    jid:from_binary(U1),
                                                    jid:from_binary(U2),
                                                    not_used, Dir, MsgXML))
                   || Dir <- [incoming, outgoing]],
                   ?assertEqual(Row,
                                wocky_db:select_row(?LOCAL_CONTEXT,
                                                    message_archive,
                                                    [id, lower_jid, upper_jid,
                                                     sent_to_lower, message],
                                                    SelectRow)),
                   ?assertEqual(1, wocky_db:count(?LOCAL_CONTEXT,
                                                  message_archive,
                                                  SelectRow))
               end)
         ]
        }
     ]
    }.

test_lookup_messages() ->
    Users = wocky_db_seed:archive_users(),
    U1 = hd(Users),
    U2 = lists:last(Users),

    { "lookup_message", setup, fun before_each/0, fun after_each/1,
      fun(Rows) -> [
        { "Lookup with no 2nd JID", [
            ?_assertEqual({error, missing_with_jid},
                          lookup_by_users(U1, undefined))
         ]
        },
        { "Lookup by user", [
            ?_assertEqual(to_ret_rows(user_msgs(U1, U2, Rows)),
                          lookup_by_users(U1, U2))
         ]
        },
        { "Lookup by time", [
            ?_assertEqual(to_ret_rows(time_msgs(5, 50,
                                                user_msgs(U1, U2, Rows))),
                          lookup_by_time(U1, U2, 5, 50)),
            ?_assertEqual(to_ret_rows(time_msgs(7, 8,
                                                user_msgs(U1, U2, Rows))),
                          lookup_by_time(U1, U2, 7, 8)),
            ?_assertEqual(to_ret_rows(time_msgs(10, undefined,
                                                user_msgs(U1, U2, Rows))),
                          lookup_by_time(U1, U2, 10, undefined)),
            ?_assertEqual(to_ret_rows(time_msgs(undefined, 30,
                                                user_msgs(U1, U2, Rows))),
                          lookup_by_time(U1, U2, undefined, 30))
         ]
        },
        { "Lookup by borders exclusive", [
            ?_assertEqual(to_ret_rows(
                            id_msgs_exclusive(?FIRST_ID(8), ?FIRST_ID(35),
                                              user_msgs(U1, U2, Rows))),
                          lookup_by_borders(U1, U2,
                                    #mam_borders{after_id = ?FIRST_ID(8),
                                                 before_id = ?FIRST_ID(35)
                                                })),
            ?_assertEqual(to_ret_rows(
                            id_msgs_exclusive(?FIRST_ID(-1), ?FIRST_ID(60),
                                              user_msgs(U1, U2, Rows))),
                          lookup_by_borders(U1, U2,
                                    #mam_borders{after_id = ?FIRST_ID(-1),
                                                 before_id = ?FIRST_ID(60)
                                                }))
         ]
        },
        { "Lookup by borders inclusive", [
            ?_assertEqual(to_ret_rows(
                            id_msgs_inclusive(?FIRST_ID(8), ?FIRST_ID(35),
                                              user_msgs(U1, U2, Rows))),
                          lookup_by_borders(U1, U2,
                                    #mam_borders{from_id = ?FIRST_ID(8),
                                                 to_id = ?FIRST_ID(35)
                                                })),
            ?_assertEqual(to_ret_rows(
                            id_msgs_inclusive(?FIRST_ID(-1), ?FIRST_ID(60),
                                              user_msgs(U1, U2, Rows))),
                          lookup_by_borders(U1, U2,
                                    #mam_borders{from_id = ?FIRST_ID(-1),
                                                 to_id = ?FIRST_ID(60)
                                                })),
            ?_assertEqual(to_ret_rows(
                            id_msgs_inclusive(?FIRST_ID(10), undefined,
                                              user_msgs(U1, U2, Rows))),
                          lookup_by_borders(U1, U2,
                                    #mam_borders{from_id = ?FIRST_ID(10)}))
         ]
        },
        { "Lookup by Index", [
            ?_assertEqual(to_ret_rows(
                            messages_from_index(16, 5,
                                                user_msgs(U1, U2, Rows))),
                          lookup_by_index(U1, U2, 16, 5)),
            ?_assertEqual(to_ret_rows(
                            messages_from_index(0, 5000,
                                                user_msgs(U1, U2, Rows))),
                          lookup_by_index(U1, U2, 0, 5000)),
            % 'undefined' should return all records from the index; much fewer
            % than 5000:
            ?_assertEqual(to_ret_rows(
                            messages_from_index(0, 5000,
                                                user_msgs(U1, U2, Rows))),
                          lookup_by_index(U1, U2, 0, undefined))
         ]
        },
        { "Lookup by RSM ID", [
            ?_assertEqual(to_ret_rows(
                            messages_from_id(?FIRST_ID(6), 10, aft,
                                             user_msgs(U1, U2, Rows))),
                            lookup_by_id(U1, U2, ?FIRST_ID(6), 10, aft)),
            ?_assertEqual(to_ret_rows(
                            messages_from_id(?FIRST_ID(30), 10, before,
                                             user_msgs(U1, U2, Rows))),
                            lookup_by_id(U1, U2, ?FIRST_ID(30), 10, before))
         ]
        }

     ] end}.

user_msgs(User1, User2, Rows) ->
    lists:filter(fun(M) -> is_user_msg(M, User1, User2) end,
                 Rows).

is_user_msg(#{lower_jid := Lower, upper_jid := Upper}, User1, User2)
  when Lower =:= User1, Upper =:= User2, User1 < User2
       -> true;
is_user_msg(#{lower_jid := Lower, upper_jid := Upper}, User1, User2)
  when Lower =:= User2, Upper =:= User1, User2 < User1
       -> true;
is_user_msg(_, _, _) -> false.

time_msgs(Start, End, Rows) ->
    lists:filter(fun(#{time := Time}) ->
                         gte(Start, Time) andalso
                         lte(End, Time)
                 end, Rows).

id_msgs_inclusive(Low, High, Rows) ->
    lists:filter(fun(#{id := ID}) ->
                         gte(Low, ID) andalso
                         lte(High, ID)
                 end, Rows).

id_msgs_exclusive(Low, High, Rows) ->
    lists:filter(fun(#{id := ID}) ->
                         gt(Low, ID) andalso
                         lt(High, ID)
                 end, Rows).

gte(undefined, _) -> true;
gte(Low, Val) ->
    Val >= Low.

lte(undefined, _) -> true;
lte(High, Val) ->
    Val =< High.

gt(undefined, _) -> true;
gt(Low, Val) ->
    Val > Low.

lt(undefined, _) -> true;
lt(High, Val) ->
    Val < High.

% This case should throw an error - don't match the return - leave that to the
% test assertion
lookup_by_users(User1, User2 = undefined) ->
    lookup_messages(ok, ?LOCAL_CONTEXT, not_used, User1, undefined, undefined,
                    undefined, undefined, undefined, User2, 10000, not_used,
                    not_used, false);

lookup_by_users(User1, User2) ->
    {ok, {_Total, _Offset, Msgs}} =
    lookup_messages(ok, ?LOCAL_CONTEXT, not_used, User1, undefined, undefined,
                    undefined, undefined, undefined, User2, 10000, not_used,
                    not_used, false),
    Msgs.

lookup_by_time(User1, User2, Start, End) ->
    {ok, {_Total, _Offset, Msgs}} =
    lookup_messages(ok, ?LOCAL_CONTEXT, not_used, User1, undefined, undefined,
                    ms_to_us(Start), ms_to_us(End), undefined, User2, 10000,
                    false, 10000, false),
    Msgs.

lookup_by_borders(User1, User2, Borders) ->
    {ok, {_Total, _Offset, Msgs}} =
    lookup_messages(ok, ?LOCAL_CONTEXT, not_used, User1, undefined, Borders,
                    undefined, undefined, undefined, User2, 10000, false, 10000,
                    false),
    Msgs.

lookup_by_index(User1, User2, Index, Max) ->
    {ok, {_Total, _Offset, Msgs}} =
    lookup_messages(ok, ?LOCAL_CONTEXT, not_used, User1,
                    #rsm_in{index = Index, max = Max}, undefined, undefined,
                    undefined, undefined, User2, Max, undefined, undefined,
                    false),
    Msgs.

lookup_by_id(User1, User2, ID, Max, Direction) ->
    {ok, {_Total, _Offset, Msgs}} =
    lookup_messages(ok, ?LOCAL_CONTEXT, not_used, User1,
                    #rsm_in{id = ID, max = Max, direction = Direction},
                    undefined, undefined, undefined, undefined, User2,
                    Max, undefined, undefined, false),
    Msgs.

ms_to_us(undefined) -> undefined;
ms_to_us(Time) -> Time * 1000.

to_ret_rows(Rows) -> [to_ret_row(R) || R <- Rows].

to_ret_row(#{id := ID, lower_jid := LowerJID, upper_jid := UpperJID,
             sent_to_lower := ToLower, message := Message}) ->
    SrcJID = src_jid(LowerJID, UpperJID, ToLower),
    {ok, MsgXML} = exml:parse(Message),
    {ID, jid:from_binary(SrcJID), MsgXML}.

src_jid(Lower, _, false) -> Lower;
src_jid(_, Upper, true) -> Upper.

nth_id(User1, User2, TargetID, Rows) ->
    first_id(
      lists:dropwhile(fun(#{id := ID, lower_jid := JID1, upper_jid := JID2}) ->
                              ID < TargetID orelse
                              (JID1 =/= User1 andalso JID2 =/= User1) orelse
                              (JID1 =/= User2 andalso JID2 =/= User2)
                      end, Rows)).

first_id([]) -> undefined;
first_id([#{id := ID} | _]) -> ID.

messages_from_index(Index, N, Rows) ->
    {_, Tail} = lists:split(min(Index, length(Rows)), Rows),
    {Ret, _} = lists:split(min(N, length(Tail)), Tail),
    Ret.

messages_from_id(TargetID, N, aft, Rows) ->
    AfterID = lists:dropwhile(fun(#{id := ID}) -> ID =< TargetID end, Rows),
    {Ret, _} = lists:split(min(N, length(AfterID)), AfterID),
    Ret;
messages_from_id(TargetID, N, before, Rows) ->
    BeforeID = lists:takewhile(fun(#{id := ID}) -> ID < TargetID end, Rows),
    SplitAt = length(BeforeID) - N,
    {_, Ret} = lists:split(max(0, SplitAt), BeforeID),
    Ret.

