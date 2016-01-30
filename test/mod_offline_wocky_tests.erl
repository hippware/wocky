%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for mod_offline_wocky.erl
-module(mod_offline_wocky_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/mod_offline.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include("wocky_db_seed.hrl").


mod_offline_wocky_test_() -> {
  "mod_offline_wocky",
  setup, fun before_all/0, fun after_all/1,
  [
    test_pop_messages(),
    test_write_messages(),
    test_message_expiry(),
    test_remove_user()
  ]
}.

before_all() ->
    ok = wocky_app:start(),
    ok = wocky_db_seed:prepare_tables(?LOCAL_CONTEXT, [offline_msg]),
    ok.

after_all(_) ->
    ok = wocky_app:stop().

before_each() ->
    {ok, Data} = wocky_db_seed:seed_table(?LOCAL_CONTEXT, offline_msg),
    [data_to_rec(D) || D <- Data].

data_to_rec(#{user := User, timestamp := TS, expire := Expire,
              from_id := From, to_id := To, packet := PacketXML}) ->
    {ok, PacketBinary} = exml:parse(PacketXML),
    #offline_msg{us = {User, ?SERVER},
                 timestamp = wocky_db:timestamp_to_now(TS),
                 expire = wocky_db:timestamp_to_now(Expire),
                 from = jid:from_binary(From),
                 to = jid:from_binary(To),
                 packet = PacketBinary}.

make_msg_recs(User, Handle, N) ->
    NowSecs = wocky_db_seed:get_nowsecs(),
    Data = wocky_db_seed:make_offline_msgs(User, Handle, NowSecs, N),
    [data_to_rec(D) || D <- Data].

after_each(_) ->
    ok = wocky_db_seed:clear_tables(?LOCAL_CONTEXT, [offline_msg]),
    ok.

test_pop_messages() ->
    { "pop_messages", setup, fun before_each/0, fun after_each/1,
      fun(Config) -> [
        { "Get a user's messages and check they're cleared correctly", [
            ?_test(
               begin
                   Msgs = lists:filter(
                            fun (#offline_msg{us = {UUID, _}}) -> UUID =:= ?BOB;
                                (_) -> false
                            end,
                            Config),
                   ?assertEqual({ok, Msgs},
                                mod_offline_wocky:pop_messages(?BOB, ?SERVER)),
                   ?assertEqual({ok, []},
                                mod_offline_wocky:pop_messages(?BOB, ?SERVER))
               end
              )
        ]},
        { "A non-existant user should have no messges", [
            ?_assertEqual({ok, []}, mod_offline_wocky:pop_messages(
                                      ?BADUSER, ?SERVER))
        ]}
    ] end}.

test_write_messages() ->
    { "write_messages", setup, fun before_each/0, fun after_each/1, [
        { "Write and retrieve a single message", [
            ?_test(
               begin
                   [Rec] = make_msg_recs(?TIM, <<"tim">>, 1),
                   ?assertEqual(ok, mod_offline_wocky:write_messages(
                                      ?TIM, ?SERVER, [Rec], unused)),
                   ?assertEqual({ok, [Rec]},
                                mod_offline_wocky:pop_messages(?TIM, ?SERVER)),
                   ?assertEqual({ok, []},
                                mod_offline_wocky:pop_messages(?TIM, ?SERVER))
               end
              )
         ]},
         { "Write and retrieve lots of randomly ordered messages", [
            ?_test(
               begin
                   Recs = make_msg_recs(?TIM, <<"tim">>, 10),
                   ShuffledRecs = shuffle_list(Recs),
                   {Recs1, Recs2} = lists:split(5, ShuffledRecs),
                   ?assertEqual(ok, mod_offline_wocky:write_messages(
                                      ?TIM, ?SERVER, Recs1, unused)),
                   ?assertEqual(ok, mod_offline_wocky:write_messages(
                                      ?TIM, ?SERVER, Recs2, unused)),
                   %% Records should be returned in chronological order:
                   ?assertEqual({ok, Recs},
                                mod_offline_wocky:pop_messages(?TIM, ?SERVER)),
                   ?assertEqual({ok, []},
                                mod_offline_wocky:pop_messages(?TIM, ?SERVER))
               end
              )
         ]}
     ]}.

test_message_expiry() ->
    { "message_expiry", setup, fun before_each/0, fun after_each/1, [
        { "Ensure messages expire at their allotted time", [
            ?_test(
               begin
                   Recs = make_msg_recs(?TIM, <<"tim">>, 10),
                   NowTS = wocky_db:now_to_timestamp(os:timestamp()),
                   ExpireTS = NowTS + 1000,
                   ShortExipreRecs =
                       [R#offline_msg{
                          expire = wocky_db:timestamp_to_now(ExpireTS)
                         } || R <- Recs],
                   ?assertEqual(ok, mod_offline_wocky:write_messages(
                                      ?TIM, ?SERVER, ShortExipreRecs, unused)),

                   timer:sleep(2000),
                   ?assertEqual({ok, []},
                                mod_offline_wocky:pop_messages(?TIM, ?SERVER))
               end
             )
        ]},
        { "Ensure messages don't expire too soon", [
            ?_test(
               begin
                   Recs = make_msg_recs(?TIM, <<"tim">>, 10),
                   NowTS = wocky_db:now_to_timestamp(os:timestamp()),
                   ExpireTS = NowTS + 3000,
                   ShortExipreRecs =
                       [R#offline_msg{
                          expire = wocky_db:timestamp_to_now(ExpireTS)
                         } || R <- Recs],
                   ?assertEqual(ok, mod_offline_wocky:write_messages(
                                      ?TIM, ?SERVER, ShortExipreRecs, unused)),
                   %% Only sleep for one second - rounding effects can mean the
                   %% expiry time can end up only being two seconds (TTL rounds
                   %% to the nearest second).
                   timer:sleep(1000),
                   ?assertEqual({ok, ShortExipreRecs},
                                mod_offline_wocky:pop_messages(?TIM, ?SERVER))
               end
             )
        ]}
    ]}.

test_remove_user() ->
    { "remove_user", setup, fun before_each/0, fun after_each/1, [
        { "Remove a user and ensure no messages remain", [
            ?_assertEqual(ok, mod_offline_wocky:remove_user(?ALICE, ?SERVER)),
            ?_assertEqual({ok, []},
                          mod_offline_wocky:pop_messages(?ALICE, ?SERVER))
        ]}
    ]}.


% Little helper function to pseudo-randomly shuffle a list of elements
shuffle_list(L) ->
    [X || {_, X} <- lists:sort([{random:uniform(), E} || E <- L])].
