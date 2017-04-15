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
         seed_data/2, maybe_seed_s3_file/2, random_conversation_list/0]).

-ifdef(TEST).
-export([archive_users/0, msg_xml_packet/1]).
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
       avatar => ?tros:make_url(Server, ?AVATAR_FILE),
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
seed_data(file_metadata, Server) ->
    [#{owner => ?ALICE, server => Server,
       id => ?AVATAR_FILE, access => <<"all">>},
     #{owner => ?ALICE, server => Server,
       id => ?MEDIA_FILE, access => <<"user:", (?BOB_B_JID)/binary>>}];
seed_data(bot, Server) ->
    [#{id => ?BOT, server => Server, title => ?BOT_TITLE,
       shortname => ?BOT_NAME, owner => ?ALICE_B_JID, description => ?BOT_DESC,
       lat => ?BOT_LAT, lon => ?BOT_LON, radius => ?BOT_RADIUS,
       address => ?BOT_ADDRESS, image => ?AVATAR_FILE, type => ?BOT_TYPE,
       visibility => ?WOCKY_BOT_VIS_WHITELIST,
       tags => ?BOT_TAGS, alerts => ?WOCKY_BOT_ALERT_DISABLED,
       updated => now, follow_me => false, follow_me_expiry => 0
      }];
seed_data(bot_share, _Server) ->
    [#{bot => <<?LOCAL_CONTEXT/binary, "/bot/", ?BOT/binary>>,
       to_jid => ?BOB_B_JID, from_jid => ?ALICE_B_JID, time => now}];
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
    Alice = #{user => ?ALICE, server => Server},
    [Alice#{id => ?BOT, version => ?HS_V_1, from_id => ?BOT_B_JID,
            stanza => ?BOT_UPDATE_STANZA, deleted => false},
     Alice#{id => ?ITEM, version => ?HS_V_2, from_id => ?BOT_B_JID,
            stanza => ?ITEM_STANZA, deleted => false},
     Alice#{id => ?ITEM2, version => ?HS_V_3, from_id => ?BOT_B_JID,
            stanza => ?ITEM_STANZA2, deleted => false} |
     [#{user => ?BOB, server => Server,
        id => ?wocky_id:new(),
        version => ?wocky_id:new(),
        from_id => ?BOT_B_JID,
        deleted => false,
        stanza => ?BOT_UPDATE_STANZA
       } ||
      _ <- lists:seq(1, ?BOB_HS_ITEM_COUNT)]
    ];
seed_data(_, _) ->
    [].

%%====================================================================
%% Data generation helpers
%%====================================================================

msg_xml_packet(Handle) ->
    <<"<message xml:lang=\"en\" type=\"chat\"
                to=\"278e4ba0-b9ae-11e5-a436-080027f70e96@localhost\">
           <body>Hi, ", Handle/binary, "</body>
       </message>">>.

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
    case application:get_env(wocky, tros_backend, undefined) of
        ?tros_s3 -> seed_s3_file(UserJID, FileID);
        _ -> ok
    end.

seed_s3_file(UserJID, FileID) ->
    {Headers, Fields} = ?tros:make_upload_response(
                          UserJID, FileID, 1000, <<"all">>,
                          #{<<"content-type">> => <<"image/png">>}),
    HeadersStr = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Headers],
    {ok, _} =
    httpc:request(put,
                  {binary_to_list(proplists:get_value(<<"url">>, Fields)),
                   HeadersStr, "image/png", crypto:strong_rand_bytes(1000)},
                  [], []).
