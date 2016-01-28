%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for mod_offline_wocky.erl
-module(mod_offline_wocky_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/mod_offline.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include("wocky_db_seed.hrl").


-record(config, {
          users,
          nowsecs,
          recs,
          maps
         }).

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

uuid(Name, Config) ->
    E = lists:keyfind(Name, 1, Config#config.users),
    element(2, E).

get_nowsecs() ->
    {Mega, Sec, _} = os:timestamp(),
    (Mega * 1000000) + Sec.

before_each() ->
    Users = [{<<"bob">>, wocky_db_user:create_id()},
             {<<"alicia">>, wocky_db_user:create_id()},
             {<<"robert">>, wocky_db_user:create_id()},
             {<<"karen">>, wocky_db_user:create_id()},
             {<<"alice">>, wocky_db_user:create_id()}],

    NowSecs = get_nowsecs(),

    MessagelessUsers = [{<<"tim">>, wocky_db_user:create_id()}],

    Values = [make_msg_structs(User, Handle, NowSecs, I) ||
              {Handle, User} <- Users, I <- lists:seq(1, 10)],

    {Maps, Recs} = lists:unzip(Values),

    Q = "INSERT INTO offline_msg
            (user, server, msg_id, timestamp, expire, from_id, to_id, packet)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?) USING TTL ?",

    ok = wocky_db:multi_query(?SERVER, Q, Maps, quorum),
    #config{users = Users ++ MessagelessUsers, nowsecs = NowSecs,
            maps = Maps, recs = Recs}.

make_msg_structs(User, Handle, NowSecs, I) ->
    RecSecs = NowSecs + I,
    TS = wocky_db:seconds_to_timestamp(RecSecs),
    MID = wocky_db_user:create_id(),
    ExpireSecs = NowSecs + 1000,
    FromJID = #jid{user = <<"from_user">>, server = <<"from_server">>, resource
                   = <<"res1">>, luser = <<"from_user">>, lserver =
                   <<"from_server">>, lresource = <<"res1">>},
    ToJID = #jid{user = <<"to_user">>, server = <<"to_server">>, resource
                   = <<"res2">>, luser = <<"to_user">>, lserver =
                   <<"to_server">>, lresource = <<"res2">>},
    PacketXML = <<"<message xml:lan=\"en\" type=\"chat\"
                    to=\"278e4ba0-b9ae-11e5-a436-080027f70e96@localhost\"
                      ><body>Hi, ", Handle/binary, "</body></message>">>,
    Map = #{user => User,
            server => ?SERVER,
            msg_id => MID,
            timestamp => TS,
            expire => wocky_db:seconds_to_timestamp(ExpireSecs),
            from_id => jid:to_binary(FromJID),
            to_id => jid:to_binary(ToJID),
            packet => PacketXML,
            '[ttl]' =>
            wocky_db:expire_to_ttl(wocky_db:timestamp_to_now(ExpireSecs * 1000))
           },
    {ok, PacketBinary} = exml:parse(PacketXML),
    Rec = #offline_msg{us = {User, ?SERVER},
                       timestamp = wocky_db:timestamp_to_now(TS),
                       expire = wocky_db:timestamp_to_now(ExpireSecs * 1000),
                       from = FromJID,
                       to = ToJID,
                       packet = PacketBinary
                      },
    {Map, Rec}.

after_each(_) ->
    ok = wocky_db_seed:clear_tables(?LOCAL_CONTEXT, [offline_msg]),
    ok.

test_pop_messages() ->
    { "pop_messages", setup, fun before_each/0, fun after_each/1,
      fun(Config) -> [
        { "Get a user's messages and check they're cleared correctly", [
            ?_test(
               begin
                   UUID = uuid(<<"bob">>, Config),
                   Msgs = lists:filter(fun
                                           (#offline_msg{us = {UUID_, ?SERVER}})
                                               -> UUID =:= UUID_;
                                           (_) -> false
                                       end,
                                       Config#config.recs),
                   ?assertEqual({ok, Msgs},
                                mod_offline_wocky:pop_messages(UUID, ?SERVER)),
                   ?assertEqual({ok, []},
                                mod_offline_wocky:pop_messages(UUID, ?SERVER))
               end
              )
        ]},
        { "A non-existant user should have no messges", [
            ?_assertEqual({ok, []}, mod_offline_wocky:pop_messages(
                                      wocky_db_user:create_id(), ?SERVER))
        ]}
    ] end}.

test_write_messages() ->
    { "write_messages", setup, fun before_each/0, fun after_each/1,
      fun(Config) -> [
        { "Write and retrieve a single message", [
            ?_test(
               begin
                   UUID = uuid(<<"tim">>, Config),
                   {_Map, Rec} = make_msg_structs(UUID, <<"tim">>,
                                                 Config#config.nowsecs, 0),
                   ?assertEqual(ok, mod_offline_wocky:write_messages(UUID,
                                                  ?SERVER, [Rec], unused)),
                   ?assertEqual({ok, [Rec]},
                                mod_offline_wocky:pop_messages(UUID, ?SERVER)),
                   ?assertEqual({ok, []},
                                mod_offline_wocky:pop_messages(UUID, ?SERVER))
               end
              )
         ]},
         { "Write and retrieve lots of randomly ordered messages", [
            ?_test(
               begin
                   UUID = uuid(<<"tim">>, Config),

                   Values = [make_msg_structs(UUID, <<"tim">>, get_nowsecs(), I)
                             || I <- lists:seq(1, 10)],
                   {_Maps, Recs} = lists:unzip(Values),

                   ShuffledRecs = shuffle_list(Recs),
                   {Recs1, Recs2} = lists:split(5, ShuffledRecs),
                   ?assertEqual(ok, mod_offline_wocky:write_messages(UUID,
                                                 ?SERVER, Recs1, unused)),
                   ?assertEqual(ok, mod_offline_wocky:write_messages(UUID,
                                                 ?SERVER, Recs2, unused)),
                   % Records should be returned in chronological order:
                   ?assertEqual({ok, Recs},
                                mod_offline_wocky:pop_messages(UUID, ?SERVER)),
                   ?assertEqual({ok, []},
                                mod_offline_wocky:pop_messages(UUID, ?SERVER))
               end
              )
         ]}
     ] end}.

test_message_expiry() ->
    { "message_expiry", setup, fun before_each/0, fun after_each/1,
      fun(Config) -> [
        { "Ensure messages expire at their allotted time", [
            ?_test(
               begin
                   UUID = uuid(<<"tim">>, Config),
                   make_msg_structs(UUID, <<"tim">>,
                                                 Config#config.nowsecs, 0),

                   Values = [make_msg_structs(UUID, <<"tim">>, get_nowsecs(), I)
                             || I <- lists:seq(1, 10)],
                   {_Maps, Recs} = lists:unzip(Values),
                   NowTS = wocky_db:now_to_timestamp(os:timestamp()),
                   ExpireTS = NowTS + 1000,
                   ShortExipreRecs = [R#offline_msg{expire =
                                            wocky_db:timestamp_to_now(ExpireTS)}
                                      || R <- Recs],
                   ?assertEqual(ok, mod_offline_wocky:write_messages(UUID,
                                     ?SERVER, ShortExipreRecs, unused)),

                   timer:sleep(2000),
                   ?assertEqual({ok, []},
                                mod_offline_wocky:pop_messages(UUID, ?SERVER))
               end
             )
        ]},
        { "Ensure messages don't expire too soon", [
            ?_test(
               begin
                   UUID = uuid(<<"tim">>, Config),
                   make_msg_structs(UUID, <<"tim">>,
                                                 Config#config.nowsecs, 0),

                   Values = [make_msg_structs(UUID, <<"tim">>, get_nowsecs(), I)
                             || I <- lists:seq(1, 10)],
                   {_Maps, Recs} = lists:unzip(Values),
                   NowTS = wocky_db:now_to_timestamp(os:timestamp()),
                   ExpireTS = NowTS + 3000,
                   ShortExipreRecs = [R#offline_msg{expire =
                                            wocky_db:timestamp_to_now(ExpireTS)}
                                      || R <- Recs],
                   ?assertEqual(ok, mod_offline_wocky:write_messages(UUID,
                                     ?SERVER, ShortExipreRecs, unused)),
                   % Only sleep for one second - rounding effects can mean the
                   % expiry time can end up only being two seconds (TTL rounds
                   % to the nearest second).
                   timer:sleep(1000),
                   ?assertEqual({ok, ShortExipreRecs},
                                mod_offline_wocky:pop_messages(UUID, ?SERVER))
               end
             )
        ]}
    ] end}.

test_remove_user() ->
    { "remove_user", setup, fun before_each/0, fun after_each/1,
      fun(Config) -> [
        { "Remove a user and ensure no messages remain", [
            ?_assertEqual(ok, mod_offline_wocky:remove_user(
                                uuid(<<"alice">>, Config), ?SERVER)),
            ?_assertEqual({ok, []},
                          mod_offline_wocky:pop_messages(
                            uuid(<<"alice">>, Config), ?SERVER))
        ]}
    ] end}.


% Little helper function to pseudo-randomly shuffle a list of elements
shuffle_list(L) ->
    [X || {_, X} <- lists:sort([{random:uniform(), E} || E <- L])].
