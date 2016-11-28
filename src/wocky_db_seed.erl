%%% @copyright 2016+ Hippware, Inc.
%%% @doc Helper API for manipulating the database during testing.
%%%
-module(wocky_db_seed).

-include_lib("ejabberd/include/jlib.hrl").
-include("wocky.hrl").
-include("wocky_db_seed.hrl").
-include("wocky_roster.hrl").
-include("wocky_bot.hrl").

-export([seed_table/2, seed_tables/2, seed_keyspace/2,
         seed_data/2, maybe_seed_s3_file/2]).

-ignore_xref([seed_table/2, seed_tables/2, seed_data/2, maybe_seed_s3_file/2]).

-ifdef(TEST).
-export([make_offline_msgs/5, get_nowsecs/0,
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
    ok = wocky_db:clear_tables(Context, [Name]),
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
       access => <<"all">>,
       metadata => #{<<"content-type">> => <<"image/png">>,
                     <<"name">> => ?FILENAME}},
     #{id => ?AVATAR_FILE2, user => ?ALICE, size => byte_size(?AVATAR_DATA2),
       chunks => [?AVATAR_CHUNK2],
       access => <<"all">>,
       metadata => #{<<"content-type">> => <<"image/png">>,
                     <<"name">> => ?FILENAME}},
     #{id => ?AVATAR_FILE3, user => ?BOB,   size => byte_size(?AVATAR_DATA3),
       chunks => [?AVATAR_CHUNK3],
       access => <<"all">>,
       metadata => #{<<"content-type">> => <<"image/png">>,
                     <<"name">> => ?FILENAME}},
     #{id => ?MEDIA_FILE, user => ?ALICE, size => byte_size(?MEDIA_DATA),
       chunks => [?MEDIA_CHUNK],
       access =>
           <<"user:", (jid:to_binary(jid:make(?BOB, Server, <<>>)))/binary>>,
       metadata => #{<<"content-type">> => <<"image/png">>,
                     <<"name">> => ?FILENAME}},
     #{id => ?GC_MEDIA_FILE, user => ?ALICE, size => byte_size(?MEDIA_DATA),
       chunks => [?MEDIA_CHUNK],
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
       address => ?BOT_ADDRESS, image => ?AVATAR_FILE, type => ?BOT_TYPE,
       visibility => ?WOCKY_BOT_VIS_WHITELIST,
       affiliates => [?BOB_B_JID],
       alerts => ?WOCKY_BOT_ALERT_DISABLED,
       updated => now, follow_me => false, follow_me_expiry => 0
      }];
seed_data(bot_name, _Server) ->
    [#{shortname => ?BOT_NAME, id => ?BOT}];
seed_data(bot_subscriber, _Server) ->
    [#{bot => ?BOT, user => ?CAROL_B_JID,
       server => ?LOCAL_CONTEXT},
     #{bot => ?BOT, user => ?KAREN_B_JID,
       server => ?LOCAL_CONTEXT}];
seed_data(bot_item, _Server) ->
    [#{id => ?ITEM, bot => ?BOT, published => ?ITEM_PUB_TIME,
       updated => ?ITEM_UPDATE_TIME, stanza => ?ITEM_STANZA,
       image => true},
     #{id => ?ITEM2, bot => ?BOT, published => ?ITEM_PUB_TIME+1,
       updated => ?ITEM_UPDATE_TIME+1, stanza => ?ITEM_STANZA2,
       image => false}];
seed_data(home_stream, Server) ->
    UserBase = #{user => ?ALICE, server => Server},
    [UserBase#{id => ?BOT, version => ?HS_V_1, from_id => ?BOT_B_JID,
               stanza => ?BOT_UPDATE_STANZA, deleted => false},
     UserBase#{id => ?ITEM, version => ?HS_V_2, from_id => ?BOT_B_JID,
               stanza => ?ITEM_STANZA, deleted => false},
     UserBase#{id => ?ITEM2, version => ?HS_V_3, from_id => ?BOT_B_JID,
               stanza => ?ITEM_STANZA2, deleted => false}];
seed_data(_, _) ->
    [].

%%====================================================================
%% Data generation helpers
%%====================================================================

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

% Set up a file on S3 with random data if S3 is enabled
maybe_seed_s3_file(UserJID, FileID) ->
    case ejabberd_config:get_local_option(tros_backend) of
        s3 -> seed_s3_file(UserJID, FileID);
        _ -> ok
    end.

seed_s3_file(UserJID, FileID) ->
    {Headers, Fields} = mod_wocky_tros_s3:make_upload_response(
                          UserJID, #jid{lserver = ?LOCAL_CONTEXT},
                          FileID, 1000,
                          <<"all">>, #{<<"content-type">> => <<"image/png">>}),
    HeadersStr = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Headers],
    {ok, _} =
    httpc:request(put,
                  {binary_to_list(proplists:get_value(<<"url">>, Fields)),
                   HeadersStr, "image/png", crypto:rand_bytes(1000)},
                  [], []).
