%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for mod_wocky_mam.erl
-module(mod_wocky_mam_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd_config.hrl").
-include("wocky_db_seed.hrl").

-import(mod_wocky_mam, [
                        archive_message_hook/9,
                        lookup_messages_hook/14,
                        do_lookup/7,
                        standard_counts/8
                       ]).

-define(FIRST_ID(N), nth_id(U1, U2, N, Rows)).

mod_wocky_mam_test_() -> {
  "mod_wocky_mam",
  setup, fun before_all/0, fun after_all/1,
  [
   test_archive_message_hook(),
   test_lookup_messages_hook(),
   test_do_lookup(),
   test_standard_counts()
  ]
 }.

before_all() ->
    meck:new(gen_mod, [passthrough]),
    meck:expect(gen_mod, get_module_opt,
                fun(global, mod_wocky_mam, message_ttl, infinity) -> infinity
                end),
    ets:new(config, [named_table, set, public, {keypos, 2}]),
    ets:insert(config, #config{key = hosts, value = [<<"localhost">>]}),
    application:ensure_started(p1_stringprep),
    ok = wocky_app:start(),
    ok = wocky_db_seed:prepare_tables(?LOCAL_CONTEXT, [message_archive]),
    ok.

after_all(_) ->
    ets:delete(config),
    meck:unload(gen_mod),
    ok = wocky_app:stop().

before_each() ->
    ok = wocky_db_seed:clear_tables(?LOCAL_CONTEXT, [message_archive]),
    {ok, Rows} = wocky_db_seed:seed_table(?LOCAL_CONTEXT, message_archive),
    Rows.

after_each(_) ->
    ok.

test_archive_message_hook() ->
    %% fixture setup code...
    {U1, U2} = users_to_test(),

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
       { "should use TTL value from module options", [
          ?_assert(meck:validate(gen_mod))
       ]},
       { "with an incoming message",
         setup,
         fun () ->
                 ok = archive_message_hook(ok, ?LOCAL_CONTEXT, ID,
                                           not_used,
                                           jid:from_binary(U1),
                                           jid:from_binary(U2),
                                           not_used, incoming, MsgXML)
         end,
         fun (_) ->
                 ok = wocky_db_seed:clear_tables(?LOCAL_CONTEXT,
                                                 [message_archive])
         end,
         [
          { "should archive the message so that it can be retrieved later", [
             ?_assertEqual(Row,
                           wocky_db:select_row(?LOCAL_CONTEXT,
                                               message_archive,
                                               [id, lower_jid, upper_jid,
                                                sent_to_lower, message],
                                               SelectRow))
          ]},
          { "should archive the message once", [
             ?_assertEqual(1, wocky_db:count(?LOCAL_CONTEXT,
                                             message_archive,
                                             SelectRow))
          ]}
         ]
       },
       { "with an outgoing message",
         setup,
         fun () ->
                 ok = archive_message_hook(ok, ?LOCAL_CONTEXT, ID,
                                           not_used,
                                           jid:from_binary(U1),
                                           jid:from_binary(U2),
                                           not_used, outgoing, MsgXML)
         end,
         fun (_) ->
                 ok = wocky_db_seed:clear_tables(?LOCAL_CONTEXT,
                                                 [message_archive])
         end,
         [
          { "should not archive the message", [
             ?_assertEqual(not_found,
                           wocky_db:select_row(?LOCAL_CONTEXT,
                                               message_archive,
                                               [id, lower_jid, upper_jid,
                                                sent_to_lower, message],
                                               SelectRow))
          ]}
         ]
        },
        { "Archiving should access TTL value from module options", [
            ?_assert(meck:validate(gen_mod))
         ]
        }
     ]
    }.

% Only do the simplest of tests here - the other cases produce test code that is
% too complex and fragile, and is effectively covered by integration tets.
test_lookup_messages_hook() ->
    {U1, _} = users_to_test(),

    { "lookup_message", setup, fun before_each/0, fun after_each/1,
      [
        { "Lookup with no 2nd JID", [
            ?_assertEqual({error, missing_with_jid},
                          lookup_messages_hook(ok, ?LOCAL_CONTEXT,
                                               not_used, U1, not_used,
                                               not_used, not_used, not_used,
                                               not_used, undefined, not_used,
                                               not_used, not_used, not_used))
         ]}
     ]}.

test_do_lookup() ->
    {U1, U2} = users_to_test(),

    { "do_lookup", setup, fun before_each/0, fun after_each/1,
      fun(Rows) ->
        UserRows = user_msgs(U1, U2, Rows),
      [
        { "with only JIDS", [
          ?_assertEqual(UserRows,
                        match_rows(
                          do_lookup(?LOCAL_CONTEXT, U1, U2,
                                    undefined, undefined,
                                    undefined, undefined)))
        ]},
        { "with a limit", [
          ?_assertEqual(limit_results(4, UserRows, aft),
                        match_rows(
                          do_lookup(?LOCAL_CONTEXT, U1, U2,
                                    undefined, undefined, 4, undefined)))
        ]},
        { "with times", [
          ?_assertEqual(time_msgs(6, 50, UserRows),
                        match_rows(
                          do_lookup(?LOCAL_CONTEXT, U1, U2,
                                    ms_to_time(6), ms_to_time(50),
                                    undefined, undefined))),
          ?_assertEqual(time_msgs(undefined, 50, UserRows),
                        match_rows(
                          do_lookup(?LOCAL_CONTEXT, U1, U2,
                                    {time, undefined}, ms_to_time(50),
                                    undefined, undefined))),
          ?_assertEqual(time_msgs(10, undefined, UserRows),
                        match_rows(
                          do_lookup(?LOCAL_CONTEXT, U1, U2,
                                    ms_to_time(10), {time, undefined},
                                    undefined, undefined)))
        ]},
        { "with time and reversed direction", [
          ?_assertEqual(rev_time_msgs(6, 50, UserRows),
                        match_rows(
                          do_lookup(?LOCAL_CONTEXT, U1, U2,
                                    ms_to_time(6), ms_to_time(50),
                                    undefined, before))),
          ?_assertEqual(rev_time_msgs(undefined, 50, UserRows),
                        match_rows(
                          do_lookup(?LOCAL_CONTEXT, U1, U2,
                                    {time, undefined}, ms_to_time(50),
                                    undefined, before)))
        ]}
    ] end}.

test_standard_counts() ->
    {U1, U2} = users_to_test(),

    { "standard_counts", setup, fun before_each/0, fun after_each/1,
      fun(Rows) ->
        UserRows = user_msgs(U1, U2, Rows),
      [
        { "No count needed", [
          ?_assertEqual({undefined, undefined},
                        standard_counts(false, ?LOCAL_CONTEXT, U1, U2, 
                                        undefined, undefined, undefined,
                                        UserRows))
        ]},
        { "Simple full count", [
          ?_assertEqual({length(UserRows), 0},
                        standard_counts(true, ?LOCAL_CONTEXT, U1, U2,
                                        undefined, undefined, undefined,
                                        UserRows))
        ]},
        { "Time restricted count", [
          ?_assertEqual({length(time_msgs(6, 50, UserRows)),
                         length(time_msgs(undefined, 5, UserRows))},
                        standard_counts(true, ?LOCAL_CONTEXT, U1, U2,
                                        ms_to_time(6), ms_to_time(50),
                                        undefined,
                                        time_msgs(6, 50, UserRows))),
          ?_assertEqual({length(time_msgs(100, undefined, UserRows)),
                         length(time_msgs(undefined, 99, UserRows))},
                        standard_counts(true, ?LOCAL_CONTEXT, U1, U2,
                                        ms_to_time(100), undefined,
                                        undefined,
                                        time_msgs(100, undefined, UserRows)))
        ]}
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

rev_time_msgs(Start, End, Rows) -> lists:reverse(time_msgs(Start, End, Rows)).
time_msgs(Start, End, Rows) ->
    lists:filter(fun(#{time := Time}) ->
                         gte(Start, Time) andalso
                         lte(End, Time)
                 end, Rows).

gte(undefined, _) -> true;
gte(Low, Val) ->
    Val >= Low.

lte(undefined, _) -> true;
lte(High, Val) ->
    Val =< High.

ms_to_time(undefined) -> {time, undefined};
ms_to_time(Time) -> {time, Time * 1000}.

limit_results(undefined, Results, _Direction) ->
    Results;
limit_results(N, Results, aft) ->
    {Ret, _} = lists:split(min(N, length(Results)), Results),
    Ret;
limit_results(N, Results, before) ->
    SplitAt = length(Results) - N,
    {_, Ret} = lists:split(max(0, SplitAt), Results),
    Ret.

match_rows(Rows) -> [match_row(R) || R <- Rows].
match_row(Row = #{time := Time}) ->
    Row#{time => uuid:get_v1_time(Time) div 1000}.

users_to_test() ->
    Users = wocky_db_seed:archive_users(),
    {hd(Users), lists:last(Users)}.
