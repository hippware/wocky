%%% @copyright 2016+ Hippware, Inc.
%%% @doc Helper API for manipulating the database during testing.
%%%
-module(wocky_db_seed).

-include_lib("ejabberd/include/jlib.hrl").
-include("wocky.hrl").
-include("wocky_db_seed.hrl").
-include("wocky_roster.hrl").
-include("wocky_bot.hrl").

-export([seed_table/2, seed_tables/2, seed_keyspace/2, seed_data/2]).

-ignore_xref([seed_table/2, seed_tables/2, seed_data/2]).

-ifdef(TEST).
-export([make_session/3, make_offline_msgs/5, get_nowsecs/0,
         archive_users/0, msg_xml_packet/1]).
-endif.


%%====================================================================
%% API
%%====================================================================

seed_tables(Context, Tables) ->
    lists:foreach(fun(T) -> {ok, _} = seed_table(Context, T) end, Tables).

seed_table(Context, Name) ->
    seed_table(Context, Name, ?SERVER).

seed_table(Context, Name, Server) ->
    Data = seed_data(Name, Server),
    seed_with_data(Context, Name, Data).

seed_with_data(Context, _Name, {Query, Rows}) ->
    lists:foreach(
      fun (Row) -> {ok, void} = wocky_db:query(Context, Query, Row, quorum) end,
      Rows),
    {ok, Rows};

seed_with_data(Context, Name, Rows) when is_list(Rows)->
    lists:foreach(
      fun (Row) -> wocky_db:insert_new(Context, Name, Row) end,
      Rows),
    {ok, Rows}.

seed_keyspace(Context, Server) ->
    lists:foreach(
      fun (Table) -> {ok, _} = seed_table(Context, Table, Server), ok end,
      wocky_db:keyspace_tables(Context)).


%%====================================================================
%% Seed data
%%====================================================================

user_data(Server) ->
    [
     #{user => ?ALICE,  server => Server,  handle => ?HANDLE,
       phone_number => ?PHONE_NUMBER, external_id => ?EXTERNAL_ID,
       avatar => tros:make_url(Server, ?AVATAR_FILE),
       roster_viewers => ?ROSTER_VIEWERS},
     #{user => ?CAROL,  server => Server,  handle => <<"carol">>,
       first_name => <<"Carol">>,
       phone_number => <<"+4567">>, external_id => <<"123456">>},
     #{user => ?BOB,    server => Server,  handle => <<"bob">>,
       phone_number => <<"+8901">>, external_id => <<"958731">>},
     #{user => ?KAREN,  server => Server,  handle => <<"karen">>,
       avatar => ?AVATAR_ID,
       first_name => <<"Karen">>, last_name => <<"Kismet">>,
       phone_number => <<"+5555">>, external_id => <<"598234">>},
     #{user => ?ROBERT, server => Server,  handle => <<"robert">>,
       last_name => <<"Robert The Bruce">>,
       phone_number => <<"+6666">>, external_id => <<"888312">>}
    ].

seed_data(handle_to_user, Server) ->
    [maps:with([user, server, handle, skip], U) || U <- user_data(Server)];
seed_data(phone_number_to_user, Server) ->
    [maps:with([user, server, phone_number, skip], U) ||
     U <- user_data(Server)];
seed_data(user, Server) ->
    [maps:without([phone_number], U#{password => ?SCRAM}) ||
     U <- user_data(Server)];
seed_data(last_activity, Server) ->
    Activity = [
        #{user => ?ALICE,  timestamp => 1000, status => <<"Not here">>},
        #{user => ?CAROL,  timestamp => 777,  status => <<"Ennui">>},
        #{user => ?BOB,    timestamp => 666,  status => <<"">>},
        #{user => ?KAREN,  timestamp => 999,  status => <<"Excited">>},
        #{user => ?ROBERT, timestamp => 888,  status => <<"Bored">>}
    ],
    lists:map(
      fun (#{timestamp := TS} = A) ->
          A#{timestamp := wocky_db:seconds_to_timestamp(TS), server => Server}
      end,
      Activity);
seed_data(session, Server) ->
    lists:flatmap(
      fun (#{user := User, sids := SIDs}) ->
          [make_session(User, Server, SID) || SID <- SIDs]
      end,
      session_sids());
seed_data(offline_msg, Server) ->
    NowSecs = get_nowsecs(),
    lists:flatmap(
      fun (#{user := User, handle := Handle}) ->
          make_offline_msgs(User, Server, Handle, NowSecs, 10)
      end,
      seed_data(user, Server));
seed_data(roster, Server) ->
    Items = [
        #{contact_jid => ?BOB_B_JID, nick => <<"bobby">>,
          subscription => <<"both">>,
          ask => <<"none">>, version => 666},
        #{contact_jid => ?CAROL_B_JID, nick => <<"carrie">>,
          subscription => <<"both">>,
          ask => <<"none">>, version => 777},
        #{contact_jid => ?ROBERT_B_JID, nick => <<"bob2">>,
          subscription => <<"both">>,
          ask => <<"none">>, version => 888},
        #{contact_jid => ?KAREN_B_JID, nick => <<"kk">>,
          subscription => <<"both">>,
          ask => <<"none">>, version => 999}
    ],
    [I#{user => ?ALICE, server => Server, groups => [<<"friends">>]}
     || I <- Items] ++
    [#{user => ?KAREN, server => Server, groups => [<<"__blocked__">>],
       contact_jid => ?TIM_B_JID, nick => <<"timbo">>,
          subscription => <<"both">>,
          ask => <<"none">>, version => 999},
     #{user => ?CAROL, server => Server, groups => [],
       contact_jid => ?TIM_B_JID, nick => <<"timbo">>,
          subscription => <<"none">>,
          ask => <<"none">>, version => 999},
     #{user => ?ROBERT, server => Server, groups => [],
       contact_jid => ?ALICE_B_JID, nick => <<"alice">>,
          subscription => <<"none">>,
          ask => <<"none">>, version => 999}
    ];
seed_data(message_archive, _Server) ->
    Rows = random_message_history(),
    Q = "INSERT INTO message_archive (id, time, user_jid, other_jid,
         outgoing, message) VALUES (?, minTimeuuid(:time), ?, ?, ?, ?)",
    {Q, Rows};
seed_data(conversation, _Server) ->
    Rows = random_conversation_list(),
    Q = "INSERT INTO conversation (id, time, user_jid, other_jid,
         outgoing, message) VALUES (?, minTimeuuid(:time), ?, ?, ?, ?)",
    {Q, Rows};
seed_data(auth_token, Server) ->
    [#{user => ?ALICE, server => Server, resource => ?RESOURCE,
       auth_token => ?TOKEN}];
seed_data(media, Server) ->
    [#{id => ?AVATAR_FILE, user => ?ALICE, size => byte_size(?AVATAR_DATA),
       chunks => [?AVATAR_CHUNK],
       purpose => <<"avatar">>,
       access => <<"all">>,
       metadata => #{<<"content-type">> => <<"image/png">>,
                     <<"name">> => ?FILENAME}},
     #{id => ?AVATAR_FILE2, user => ?ALICE, size => byte_size(?AVATAR_DATA2),
       chunks => [?AVATAR_CHUNK2],
       purpose => <<"avatar">>,
       access => <<"all">>,
       metadata => #{<<"content-type">> => <<"image/png">>,
                     <<"name">> => ?FILENAME}},
     #{id => ?AVATAR_FILE3, user => ?BOB,   size => byte_size(?AVATAR_DATA3),
       chunks => [?AVATAR_CHUNK3],
       purpose => <<"avatar">>,
       access => <<"all">>,
       metadata => #{<<"content-type">> => <<"image/png">>,
                     <<"name">> => ?FILENAME}},
     #{id => ?MEDIA_FILE, user => ?ALICE, size => byte_size(?MEDIA_DATA),
       chunks => [?MEDIA_CHUNK],
       purpose => <<"message_media">>,
       access =>
           <<"user:", (jid:to_binary(jid:make(?BOB, Server, <<>>)))/binary>>,
       metadata => #{<<"content-type">> => <<"image/png">>,
                     <<"name">> => ?FILENAME}},
     #{id => ?GC_MEDIA_FILE, user => ?ALICE, size => byte_size(?MEDIA_DATA),
       chunks => [?MEDIA_CHUNK],
       purpose => <<"group_chat_media">>,
       access => <<"members:", (jid:to_binary(?GROUP_CHAT_JID))/binary>>,
       metadata => #{<<"content-type">> => <<"image/png">>,
                     <<"name">> => ?FILENAME}}];
seed_data(media_data, _Server) ->
    [#{chunk_id => ?AVATAR_CHUNK,   file_id => ?AVATAR_FILE,
       data => ?AVATAR_DATA},
     #{chunk_id => ?AVATAR_CHUNK2,  file_id => ?AVATAR_FILE2,
       data => ?AVATAR_DATA2},
     #{chunk_id => ?AVATAR_CHUNK3,  file_id => ?AVATAR_FILE3,
       data => ?AVATAR_DATA3},
     #{chunk_id => ?MEDIA_CHUNK,    file_id => ?MEDIA_FILE,
       data => ?MEDIA_DATA},
     #{chunk_id => ?GC_MEDIA_CHUNK, file_id => ?GC_MEDIA_FILE,
       data => ?MEDIA_DATA}];
seed_data(privacy, Server) ->
    [#{user => ?CAROL, server => Server,
       lists => [?PRIVACY_LIST1, ?PRIVACY_LIST2]},
     #{user => ?KAREN, server => Server,
       lists => []}];
seed_data(privacy_item, Server) ->
    [#{user => ?CAROL, server => Server, list => ?PRIVACY_LIST1,
       id => ?PRIVACY_ITEM1, type => <<"jid">>,
       value => jid:to_binary(jid:make(?KAREN, Server, <<>>)),
       action => false, item_order => 1, match_all => true,
       match_iq => false, match_message => false,
       match_presence_in => false, match_presence_out => false},
     #{user => ?CAROL, server => Server, list => ?PRIVACY_LIST1,
       id => ?PRIVACY_ITEM2, type => <<"jid">>,
       value => jid:to_binary(jid:make(?KAREN, Server, <<>>)),
       action => false, item_order => 2, match_all => false,
       match_iq => true, match_message => false,
       match_presence_in => false, match_presence_out => false},
     #{user => ?CAROL, server => Server, list => ?PRIVACY_LIST2,
       id => ?PRIVACY_ITEM3, type => <<"subscription">>,
       value => <<"both">>,
       action => false, item_order => 1, match_all => false,
       match_iq => false, match_message => true,
       match_presence_in => false, match_presence_out => true}];
seed_data(group_chat, _Server) ->
    [#{id => ?GROUP_CHAT, owner => ?ALICE,
       participants =>
       [jid:to_binary(?ALICE_JID),
        jid:to_binary(?BOB_JID)],
       title => ?CHAT_TITLE},
     #{id => ?GROUP_CHAT2, owner => ?TIM,
       participants =>
       [jid:to_binary(?TIM_JID),
        jid:to_binary(?KAREN_JID)],
       title => ?CHAT_TITLE}];
seed_data(bot, Server) ->
    [#{id => ?BOT, server => Server, title => ?BOT_TITLE,
       shortname => ?BOT_NAME, owner => ?ALICE_B_JID, description => ?BOT_DESC,
       lat => ?BOT_LAT, lon => ?BOT_LON, radius => ?BOT_RADIUS,
       visibility => ?WOCKY_BOT_VIS_WHITELIST,
       affiliates => [?BOB_B_JID],
       alerts => ?WOCKY_BOT_ALERT_DISABLED
      }];
seed_data(bot_name, _Server) ->
    [#{shortname => ?BOT_NAME, id => ?BOT}];
seed_data(bot_subscriber, _Server) ->
    [#{bot => ?BOT, user => ?CAROL_B_JID, follow => false},
     #{bot => ?BOT, user => ?KAREN_B_JID, follow => true}];
seed_data(bot_item, _Server) ->
    [#{id => ?ITEM, bot => ?BOT, published => ?ITEM_PUB_TIME,
       updated => ?ITEM_UPDATE_TIME, stanza => ?ITEM_STANZA}];
seed_data(_, _) ->
    [].

%% Static SID data that can be used to populate the session
%% table. We are using static SIDs rather than radomly generated values so that
%% we can populate both tables without having to memoize the random data.
session_sids() -> [
    #{user => ?ALICE,  sids => [{{1, 2, 3},    list_to_pid("<0.2319.0>")},
                                {{4, 5, 6},    list_to_pid("<0.2321.0>")},
                                {{7, 8, 9},    list_to_pid("<0.2323.0>")},
                                {{10, 11, 12}, list_to_pid("<0.2325.0>")},
                                {{13, 14, 15}, list_to_pid("<0.2327.0>")}]},
    #{user => ?CAROL,  sids => [{{16, 17, 18}, list_to_pid("<0.2456.0>")},
                                {{19, 20, 21}, list_to_pid("<0.2457.0>")},
                                {{22, 23, 24}, list_to_pid("<0.2458.0>")},
                                {{25, 26, 27}, list_to_pid("<0.2459.0>")},
                                {{28, 29, 30}, list_to_pid("<0.2460.0>")}]},
    #{user => ?BOB,    sids => [{{31, 32, 33}, list_to_pid("<0.2466.0>")},
                                {{34, 35, 36}, list_to_pid("<0.2467.0>")},
                                {{37, 38, 39}, list_to_pid("<0.2468.0>")},
                                {{40, 41, 42}, list_to_pid("<0.2469.0>")},
                                {{43, 44, 45}, list_to_pid("<0.2470.0>")}]},
    #{user => ?KAREN,  sids => [{{46, 47, 48}, list_to_pid("<0.2472.0>")},
                                {{49, 50, 51}, list_to_pid("<0.2473.0>")},
                                {{52, 53, 54}, list_to_pid("<0.2474.0>")},
                                {{55, 56, 57}, list_to_pid("<0.2475.0>")},
                                {{58, 59, 60}, list_to_pid("<0.2476.0>")}]},
    #{user => ?ROBERT, sids => [{{61, 62, 63}, list_to_pid("<0.2478.0>")},
                                {{64, 65, 66}, list_to_pid("<0.2479.0>")},
                                {{67, 68, 69}, list_to_pid("<0.2480.0>")},
                                {{70, 71, 72}, list_to_pid("<0.2481.0>")},
                                {{73, 74, 75}, list_to_pid("<0.2482.0>")}]}
].


%%====================================================================
%% Data generation helpers
%%====================================================================

make_session(User, Server, SID) ->
    #{user => User,
      sid => term_to_binary(SID),
      server => Server,
      node => node(),
      jid_user => User,
      jid_server => Server,
      jid_resource => fake_resource(),
      priority => random_priority(),
      info => term_to_binary(session_info())}.

fake_resource() ->
    integer_to_binary(erlang:unique_integer()).

random_priority() ->
    case rand:uniform(11) of
        11 -> -1;
        N -> N
    end.

session_info() ->
    [{ip, {{127, 0, 0, 1}, rand:uniform(65536)}},
     {conn, c2s_tls},
     {auth_module, ejabberd_auth_wocky}].

get_nowsecs() ->
    {Mega, Sec, _} = os:timestamp(),
    (Mega * 1000000) + Sec.

make_offline_msgs(User, Server, Handle, NowSecs, N) ->
    [make_offline_msg(User, Server, Handle, NowSecs, I) ||
     I <- lists:seq(1, N)].

make_offline_msg(User, Server, Handle, NowSecs, I) ->
    ExpireSecs = NowSecs + 1000,
    FromJID = jid:make(<<"from_user">>, <<"from_server">>, <<"res1">>),
    ToJID = jid:make(<<"to_user">>, <<"to_server">>, <<"res2">>),
    #{user => User,
      server => Server,
      msg_id => wocky_db:create_id(),
      timestamp => wocky_db:seconds_to_timestamp(NowSecs + I),
      expire => wocky_db:seconds_to_timestamp(ExpireSecs),
      from_id => jid:to_binary(FromJID),
      to_id => jid:to_binary(ToJID),
      packet => msg_xml_packet(Handle),
      '[ttl]' => ts_to_ttl(ExpireSecs)}.

msg_xml_packet(Handle) ->
    <<"<message xml:lang=\"en\" type=\"chat\"
                to=\"278e4ba0-b9ae-11e5-a436-080027f70e96@localhost\">
           <body>Hi, ", Handle/binary, "</body>
       </message>">>.

ts_to_ttl(TS) ->
    wocky_db:expire_to_ttl(wocky_db:timestamp_to_now(TS * 1000)).

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
