%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for ejabberd_sm_wocky.erl
-module(ejabberd_sm_wocky_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/ejabberd_config.hrl").

-define(SERVER, <<"localhost">>).

ejabberd_sm_wocky_test_() -> {
  "ejabberd_sm_wocky",
  setup, fun before_all/0, fun after_all/1,
  [
   test_get_sessions(),
   test_create_session(),
   test_total_count(),
   test_unique_count(),
   test_delete_session(),
   test_cleanup()
  ]
}.

before_all() ->
    ets:new(config, [named_table, set, public, {keypos, 2}]),
    ets:insert(config, #config{key = hosts, value = [<<"localhost">>]}),
    clear_tables(),
    ok = wocky_app:start().

after_all(_) ->
    ets:delete(config),
    ok = wocky_app:stop().

before_each() ->
    Users = [{<<"bob">>, wocky_db_user:create_id()},
             {<<"alicia">>, wocky_db_user:create_id()},
             {<<"robert">>, wocky_db_user:create_id()},
             {<<"karen">>, wocky_db_user:create_id()},
             {<<"alice">>, wocky_db_user:create_id()}],
    Sessions = [make_session(ID) || _ <- lists:seq(1, 5), {_, ID} <- Users],
    [write_session(S) || S <- Sessions],
    Sessions.

after_each(_) ->
    clear_tables(),
    ok.

fake_now() ->
    list_to_tuple([erlang:unique_integer([positive, monotonic])
                   || _ <- lists:seq(1, 3)]).

make_session(ID) ->
    SIDNow = fake_now(),
    SIDPID = spawn(fun() -> ok end), % Unique(ish) PID
    USR = {ID, ?SERVER, integer_to_binary(erlang:unique_integer())},
    US = {ID, ?SERVER},
    Priority = case random:uniform(11) of
                   11 -> undefined;
                   N -> N
               end,
    Info = [{ip, {{127, 0, 0, 1}, random:uniform(65536)}},
            {conn, c2s_tls},
            {auth_module, ejabberd_auth_wocky}],
    #session{
       sid = {SIDNow, SIDPID}, usr = USR, us = US,
       priority = Priority, info = Info}.

write_session(#session{sid = Sid = {_, Pid}, us = {User, Server},
                       usr = {JIDUser, JIDServer, JIDResource},
                       priority = Priority, info = Info}) ->
    Q1 = "INSERT INTO session (sid, node, user, server, jid_user, jid_server,
        jid_resource, priority, info) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
    V1 = #{sid => term_to_binary(Sid), user => User, server => Server,
           node => node(Pid),
           jid_user => JIDUser, jid_server => JIDServer,
           jid_resource => JIDResource,
           priority => case Priority of undefined -> -1;
                                        N -> N
                       end,
           info => term_to_binary(Info)},
    Q2 = "UPDATE user_to_sids SET sids = sids + ? WHERE jid_user = ?",
    V2 = #{jid_user => JIDUser, sids => [term_to_binary(Sid)]},
    {ok, void} = wocky_db:batch_query(?SERVER, [{Q1, V1}, {Q2, V2}], logged,
                                      quorum).

session_to_ses_tuple(#session{sid = SID, usr = USR,
                              priority = Priority, info = Info}) ->
    {USR, SID, Priority, Info}.

clear_tables() ->
    {ok, _} = wocky_db:query(?SERVER, <<"TRUNCATE session">>, quorum),
    {ok, _} = wocky_db:query(?SERVER, <<"TRUNCATE user_to_sids">>, quorum),
    ok.

test_get_sessions() ->
    { "get_sessions", setup, fun before_each/0, fun after_each/1,
      fun(Sessions) -> [
        { "Get sessions by server", [
          ?_test(
             begin
                 SessTuples = [session_to_ses_tuple(S) || S <- Sessions],
                 ?assertEqual(lists:sort(SessTuples),
                           lists:sort(ejabberd_sm_wocky:get_sessions(?SERVER)))
             end)
        ]},
        { "Get sessions by server + user", [
          ?_test(
             begin
                 US = {User, _} = (lists:nth(11, Sessions))#session.us,
                 Expected = lists:filter(fun(S) -> S#session.us =:= US end,
                                         Sessions),
                 ?assertEqual(lists:sort(Expected),
                              lists:sort(ejabberd_sm_wocky:get_sessions(User,
                                                                ?SERVER)))
             end)
        ]},
        { "Invalid user should return an empty list", [
             ?_assertEqual([], ejabberd_sm_wocky:get_sessions(
                                 wocky_db_user:create_id(), ?SERVER))
        ]},
        { "Get sessions by server + user + resource", [
          ?_test(
             begin
                 S = lists:nth(18, Sessions),
                 {User, _, Resource} = S#session.usr,
                 ?assertEqual([S], ejabberd_sm_wocky:get_sessions(User, ?SERVER,
                                                                Resource))
             end)
        ]}

    ] end}.

test_total_count() ->
    { "total_count",  setup, fun before_each/0, fun after_each/1, [
        { "Count total sessions", [
            ?_assertEqual(25, ejabberd_sm_wocky:total_count())
        ]}
    ]}.

test_unique_count() ->
    { "total_count",  setup, fun before_each/0, fun after_each/1, [
        { "Count \"unique sessions\" (sessions owned by unique users)", [
            ?_assertEqual(5, ejabberd_sm_wocky:unique_count())
        ]}
    ]}.

test_create_session() ->
    { "create_session", setup, fun before_each/0, fun after_each/1,
      [
        { "Create session", [
            ?_test(
               begin
                   ID = wocky_db_user:create_id(),
                   Session = make_session(ID),
                   Resource = element(3, Session#session.usr),
                   ?assertEqual(ok, ejabberd_sm_wocky:create_session(
                                      ID, ?SERVER, Resource, Session)),
                   ?assertEqual([Session], ejabberd_sm_wocky:get_sessions(
                                             ID, ?SERVER)),
                   ?assertEqual(26, ejabberd_sm_wocky:total_count()),
                   ?assertEqual(6, ejabberd_sm_wocky:unique_count())
               end
              )
        ]}
     ]}.

test_delete_session() ->
    { "delete_session", setup, fun before_each/0, fun after_each/1,
      fun(Sessions) -> [
        { "Delete sessions", [
          ?_test(
             begin
                 S = lists:nth(6, Sessions),
                 {User, _, Resource} = S#session.usr,
                 ?assertEqual(ok, ejabberd_sm_wocky:delete_session(
                                    S#session.sid, User, ?SERVER, Resource)),
                 ?assertEqual(24, ejabberd_sm_wocky:total_count()),
                 ?assertEqual(5, ejabberd_sm_wocky:unique_count()),
                 ?assertEqual(lists:sort(
                                [session_to_ses_tuple(Sess)
                                 || Sess <- Sessions -- [S]]),
                              lists:sort(ejabberd_sm_wocky:get_sessions(
                                           ?SERVER)))
             end
            )
        ]}
     ] end}.

test_cleanup() ->
    { "create_session", setup, fun before_each/0, fun after_each/1,
      [
        { "Cleanup node sessions", [
          ?_assertEqual(ok, ejabberd_sm_wocky:cleanup(node())),
          ?_assertEqual(0, ejabberd_sm_wocky:total_count())
        ]}
    ]}.
