%%% @copyright 2015+ Hippware, Inc.
%%% @doc Test suite for ejabberd_sm_wocky.erl
-module(ejabberd_sm_wocky_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/ejabberd_config.hrl").
-include("wocky_db_seed.hrl").

-import(ejabberd_sm_wocky, [get_sessions/1, get_sessions/2, get_sessions/3,
                            create_session/4, delete_session/4, cleanup/1,
                            total_count/0, unique_count/0]).


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
    ok = wocky_app:start(),
    ok = wocky_db_seed:prepare_tables(?LOCAL_CONTEXT, [session, user_to_sids]),
    ok.

after_all(_) ->
    ets:delete(config),
    ok = wocky_app:stop().

before_each() ->
    {ok, SessData} = wocky_db_seed:seed_table(?LOCAL_CONTEXT, session),
    {ok, _} = wocky_db_seed:seed_table(?LOCAL_CONTEXT, user_to_sids),
    [data_to_rec(Sess) || Sess <- SessData].

after_each(_) ->
    ok = wocky_db_seed:clear_tables(?LOCAL_CONTEXT, [session, user_to_sids]),
    ok.

data_to_rec(#{user := User, server := Server, sid := SID, priority := Priority,
              info := Info, jid_user := JU, jid_server := JS,
              jid_resource := JR}) ->
    #session{sid = binary_to_term(SID),
             usr = {JU, JS, JR},
             us = {User, Server},
             priority = priority(Priority),
             info = binary_to_term(Info)}.

priority(-1) -> undefined;
priority(N) -> N.

session_to_ses_tuple(#session{sid = SID, usr = USR,
                              priority = Priority, info = Info}) ->
    {USR, SID, Priority, Info}.

test_get_sessions() ->
    { "get_sessions", setup, fun before_each/0, fun after_each/1,
      fun(Sessions) -> [
        { "Get sessions by server", [
          ?_test(
             begin
                 SessTuples = [session_to_ses_tuple(S) || S <- Sessions],
                 ?assertEqual(lists:sort(SessTuples),
                           lists:sort(get_sessions(?SERVER)))
             end)
        ]},
        { "Get sessions by server + user", [
          ?_test(
             begin
                 US = {User, _} = (lists:nth(11, Sessions))#session.us,
                 Expected = lists:filter(fun(S) -> S#session.us =:= US end,
                                         Sessions),
                 ?assertEqual(lists:sort(Expected),
                              lists:sort(get_sessions(User, ?SERVER)))
             end)
        ]},
        { "Invalid user should return an empty list", [
             ?_assertEqual([], get_sessions(?BADUSER, ?SERVER))
        ]},
        { "Get sessions by server + user + resource", [
          ?_test(
             begin
                 S = lists:nth(18, Sessions),
                 {User, _, Resource} = S#session.usr,
                 ?assertEqual([S], get_sessions(User, ?SERVER, Resource))
             end)
        ]}

    ] end}.

test_total_count() ->
    { "total_count",  setup, fun before_each/0, fun after_each/1, [
        { "Count total sessions", [
            ?_assertEqual(25, total_count())
        ]}
    ]}.

test_unique_count() ->
    { "total_count",  setup, fun before_each/0, fun after_each/1, [
        { "Count \"unique sessions\" (sessions owned by unique users)", [
            ?_assertEqual(5, unique_count())
        ]}
    ]}.

test_create_session() ->
    Data = wocky_db_seed:make_session(?NEWUSER),
    Session = data_to_rec(Data),
    Resource = element(3, Session#session.usr),
    { "create_session", setup, fun before_each/0, fun after_each/1, [
        { "Create session", [
            ?_assertEqual(ok, create_session(
                                ?NEWUSER, ?SERVER, Resource, Session)),
            ?_assertEqual([Session], get_sessions(?NEWUSER, ?SERVER)),
            ?_assertEqual(26, total_count()),
            ?_assertEqual(6, unique_count())
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
                 ?assertEqual(ok, delete_session(
                                    S#session.sid, User, ?SERVER, Resource)),
                 ?assertEqual(24, total_count()),
                 ?assertEqual(5, unique_count()),
                 ?assertEqual(lists:sort(
                                [session_to_ses_tuple(Sess)
                                 || Sess <- Sessions -- [S]]),
                              lists:sort(get_sessions(?SERVER)))
             end
            )
        ]}
     ] end}.

test_cleanup() ->
    { "create_session", setup, fun before_each/0, fun after_each/1,
      [
        { "Cleanup node sessions", [
          ?_assertEqual(ok, cleanup(node())),
          ?_assertEqual(0, total_count())
        ]}
    ]}.
