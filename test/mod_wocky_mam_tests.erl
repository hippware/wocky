%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for mod_wocky_mam.erl
-module(mod_wocky_mam_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd_config.hrl").
-include("wocky_db_seed.hrl").

-import(mod_wocky_mam, [
                        archive_message_hook/9,
                        do_lookup/7,
                        standard_counts/8
                       ]).

-define(FIRST_ID(N), nth_id(U1, U2, N, Rows)).

mod_wocky_mam_test_() ->
 {
  "mod_wocky_mam",
  setup, fun before_all/0, fun after_all/1,
  [
   test_archive_message_hook(),
   test_do_lookup(),
   test_standard_counts()
  ]
 }.

before_all() ->
    meck:new(gen_mod, [passthrough]),
    meck:expect(gen_mod, get_module_opt,
                fun(global, mod_wocky_mam, message_archive_ttl, infinity) ->
                        infinity
                end),
    ok = wocky_db:prepare_tables(?LOCAL_CONTEXT, [message_archive,
                                                       conversation]),
    ok.

after_all(_) ->
    meck:unload().

before_each() ->
    ok = wocky_db:clear_tables(?LOCAL_CONTEXT, [message_archive,
                                                     conversation
                                                    ]),
    {ok, _} = wocky_db_seed:seed_table(?LOCAL_CONTEXT, conversation),
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
    Row = #{id => ID, user_jid => jid:to_binary(U1),
            other_jid => jid:to_binary(U2),
            message => MsgBin},
    SelectRow = #{user_jid => jid:to_binary(U1)},

    { "archive_message", [
       { "should use TTL value from module options", [
          ?_assert(meck:validate(gen_mod))
       ]},
       { "with an incoming message",
         setup,
         fun () ->
                 ok = archive_message_hook(ok, ?LOCAL_CONTEXT, ID,
                                           not_used,
                                           U1, U2,
                                           not_used, incoming, MsgXML)
         end,
         fun (_) ->
                 ok = wocky_db:clear_tables(?LOCAL_CONTEXT,
                                                 [message_archive])
         end,
         [
          { "should archive the message so that it can be retrieved later", [
             ?_assertEqual(Row#{outgoing => false},
                           wocky_db:select_row(?LOCAL_CONTEXT,
                                               message_archive,
                                               [id, user_jid, other_jid,
                                                outgoing, message],
                                                SelectRow))
          ]},
          { "should archive the message once for this user", [
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
                                           not_used, U1, U2,
                                           not_used, outgoing, MsgXML)
         end,
         fun (_) ->
                 ok = wocky_db:clear_tables(?LOCAL_CONTEXT,
                                                 [message_archive])
         end,
         [
          { "should archive the message so that it can be retrieved later", [
             ?_assertEqual(Row#{outgoing => true},
                           wocky_db:select_row(?LOCAL_CONTEXT,
                                               message_archive,
                                               [id, user_jid, other_jid,
                                                outgoing, message],
                                                SelectRow))
          ]},
          { "should archive the message once for this user", [
             ?_assertEqual(1, wocky_db:count(?LOCAL_CONTEXT,
                                             message_archive,
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


user_msgs(UserJID, OtherJID, Rows) ->
    lists:filter(fun(M) -> is_user_msg(M,
                                       jid:to_binary(UserJID),
                                       jid:to_binary(OtherJID)) end,
                 Rows).

is_user_msg(#{user_jid := UserJID, other_jid := OtherJID}, UserJID, OtherJID) ->
    true;
is_user_msg(_, _, _) ->
    false.

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
    Row#{time => uuid:get_v1_time(uuid:string_to_uuid(Time)) div 1000}.

users_to_test() ->
    Users = wocky_db_seed:archive_users(),
    {jid:from_binary(hd(Users)), jid:from_binary(lists:last(Users))}.
