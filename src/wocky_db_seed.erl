%%% @copyright 2016+ Hippware, Inc.
%%% @doc Helper API for manipulating the database during testing.
%%%
-module(wocky_db_seed).

-include_lib("ejabberd/include/jlib.hrl").
-include("wocky.hrl").
-include("wocky_db_seed.hrl").

-export([bootstrap/0, bootstrap/1, create_schema/0, create_schema_for/1,
         foreach_table/3, recreate_table/2, create_table_indexes/2,
         create_table_views/2, drop_table_views/2, seed_table/2, seed_tables/2,
         seed_keyspace/1, prepare_tables/2, clear_tables/2]).

-export([make_session/1, make_session/2, fake_sid/0, fake_now/0, fake_pid/0,
         fake_resource/0, random_priority/0, session_info/0, sjid/1, jid/3,
         make_offline_msgs/4, make_offline_msg/4, get_nowsecs/0,
         archive_users/0, msg_xml_packet/1]).


%%====================================================================
%% Helpers
%%====================================================================

bootstrap() ->
    bootstrap(shared),
    bootstrap(?LOCAL_CONTEXT).

bootstrap(Context) ->
    create_schema_for(Context),
    seed_keyspace(Context).

create_schema() ->
    create_schema_for(shared),
    create_schema_for(?LOCAL_CONTEXT).

create_schema_for(Context) ->
    prepare_tables(Context, keyspace_tables(Context)).

foreach_table(Context, Fun, Tables) ->
    lists:foreach(fun (Table) -> ok = Fun(Context, Table) end, Tables).

recreate_table(Context, Name) ->
    ok = drop_table_views(Context, Name),
    ok = wocky_db:drop(Context, table, Name),
    ok = wocky_db:create_table(Context, table_definition(Name)),
    ok = create_table_indexes(Context, Name),
    ok = create_table_views(Context, Name),
    ok.

create_table_indexes(Context, Table) ->
    lists:foreach(
      fun (IdxKeys) -> ok = wocky_db:create_index(Context, Table, IdxKeys) end,
      table_indexes(Table)).

create_table_views(Context, Table) ->
    lists:foreach(
      fun ({Name, Columns, Keys, OrderBy}) ->
          ok = wocky_db:create_view(Context, Name, Table,
                                    Columns, Keys, OrderBy)
      end,
      table_views(Table)).

drop_table_views(Context, Table) ->
    lists:foreach(
      fun ({Name, _, _, _}) ->
          ok = wocky_db:drop(Context, 'materialized view', Name)
      end,
      table_views(Table)).

seed_tables(Context, Tables) ->
    lists:foreach(fun(T) -> {ok, _} = seed_table(Context, T) end, Tables).

seed_table(Context, Name) ->
    Data = seed_data(Name),
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

seed_keyspace(Context) ->
    foreach_table(
      Context,
      fun (_, Table) -> {ok, _} = seed_table(Context, Table), ok end,
      keyspace_tables(Context)).

prepare_tables(Context, Tables) ->
    ok = wocky_db:create_keyspace(Context, simple, 1),
    ok = foreach_table(Context, fun recreate_table/2, Tables),
    flush_cqerl_prepared_query_cache().

clear_tables(Context, Tables) ->
    foreach_table(Context, fun wocky_db:truncate/2, Tables).


%% This is an incredibly ugly hack to work around a problem in cqerl.
%% When we drop tables the C* server discards all cached prepared queries
%% related to that table. cqerl doesn't do this, and it does not handle the
%% error returned from the server that says "this prepared query doesn't exist".
%% TODO: Fix cqerl so that it properly handles the server message indicating
%% that a prepared query no longer exists in the cache. Example:
%% `{error, {9472, <<"Prepared query with ID a458ba918d03a5959c2fb8498882cfc5
%% not found (either the query was not prepared on this host (maybe the host
%% has been restarted?) or you have prepared too many queries and it has been
%% evicted from the internal cache)">>,
%% <<164,88,186,145,141,3,165,149,156,47,184,73,136,130,207,197>>}}'
flush_cqerl_prepared_query_cache() ->
    ok = supervisor:terminate_child(cqerl_sup, cqerl_cache),
    {ok, _} = supervisor:restart_child(cqerl_sup, cqerl_cache),
    ok.


%%====================================================================
%% Schema definitions
%%====================================================================

keyspace_tables(shared) -> [
    handle_to_user,
    phone_number_to_user
];
keyspace_tables(_) -> [
    user,
    last_activity,
    offline_msg,
    roster,
    session,
    media,
    media_data,
    message_archive,
    auth_token
].

%% A lookup table that maps globally unique handle to user account id
table_definition(handle_to_user) ->
    #table_def{
       name = handle_to_user,
       columns = [
           {user, timeuuid},
           {server, text},
           {handle, text}
       ],
       primary_key = handle
    };

%% A lookup table that maps globally unique phone number to user account id
table_definition(phone_number_to_user) ->
    #table_def{
       name = phone_number_to_user,
       columns = [
           {user, timeuuid},
           {server, text},
           {phone_number, text}
       ],
       primary_key = phone_number
    };

%% Table for storing the details of a user's account
table_definition(user) ->
    #table_def{
       name = user,
       columns = [
           {user, timeuuid},       % User ID (userpart of JID)
           {server, text},         % User Server (domainpart of JID)
           {handle, text},         % User handle (as seen by other users)
           {password, text},       % Password hash
           {phone_number, text},   % User's phone number
           {avatar, timeuuid},     % UUID of file containing user's avatar
           {first_name, text},     % User's first name
           {last_name, text},      % User's last name
           {email, text},          % User's email address
           {auth_user, text}       % The user ID received from Twitter Digits
       ],
       primary_key = user
    };

%% Table for storing details of users' last activty on the server. This is
%% updated only when a user logs out or disconnects
table_definition(last_activity) ->
    #table_def{
       name = last_activity,
       columns = [
           {user, timeuuid},       % User ID (userpart of JID)
           {server, text},         % User Server (domainpart of JID)
           {timestamp, timestamp}, % Timestamp of last user logoff
           {status, text}          % Text set in last user presence
                                   % with type of "unavailable"
       ],
       primary_key = user
    };

%% Table for storing messages sent to a user while they're offline
table_definition(offline_msg) ->
    #table_def{
       name = offline_msg,
       columns = [
           {user, timeuuid},       % User ID (userpart of JID)
           {server, text},         % User Server (domainpart of JID)
           {msg_id, timeuuid},     % Unique message ID
           {timestamp, timestamp}, % Message timestamp
           {expire, timestamp},    % Message expiry (as timestamp)
           {from_id, text},        % Sending user JID
           {to_id, text},          % Receiving user JID
           {packet, text}          % Full XML of <message> element
       ],
       primary_key = [user, timestamp, msg_id],
       order_by = [{timestamp, asc}]
    };

%% Table for storing a user's roster
table_definition(roster) ->
    #table_def{
       name = roster,
       columns = [
           {user, timeuuid},       % User ID (userpart of JID)
           {server, text},         % User Server (domainpart of JID)
           {contact, text},        % Bare JID for contact
           {active, boolean},      % True if the roster item is not deleted
           {nick, text},           % Display name for contact chosen by the user
           {groups, {set, text}},  % List of groups the contact belongs to
           {ask, text},            % Status if the item is pending approval
           {askmessage, text},     % Message to be used when getting approval
           {subscription, text},   % Subscription state of the roster item
           {version, timestamp}    % Timestamp indicating when the roster item
                                   % was last updated
       ],
       primary_key = [user, contact]
    };

%% Table for storing transient data for active user sessions
table_definition(session) ->
    #table_def{
       name = session,
       columns = [
           {sid, blob},            % Session ID
           {node, text},           % Node handling the active session
           {user, timeuuid},       % User ID (userpart of JID)
           {server, text},         % User Server (domainpart of JID)
           {jid_user, timeuuid},   % Provided JID userpart
           {jid_server, text},     % Provided JID domainpart
           {jid_resource, blob},   % Provided JID resourcepart
           {priority, int},        % Session priority
           {info, blob}            % Session info
       ],
       primary_key = [sid, jid_user]
    };

%% Lookup table mapping user ids to a list of session ids
table_definition(user_to_sids) ->
    #table_def{
       name = user_to_sids,
       columns = [
           {jid_user, timeuuid},   % User ID (userpart of JID)
           {sids, {set, blob}}     % Set of session IDs
       ],
       primary_key = jid_user
    };

%% Francus file-store metadata table
table_definition(media) ->
    #table_def{
       name = media,
       columns = [
           {id, timeuuid},         % ID of the file
           {user, timeuuid},       % User ID of the file owner
           {size, int},            % File size in bytes
           {metadata, {map, text, text}}, % General purpose metadata field
           {chunks, {list, timeuuid}} % Ordered list of media_data table
                                      % chunks comprising the file
       ],
       primary_key = id
    };

%% Franks file-store data table
table_definition(media_data) ->
    #table_def{
       name = media_data,
       columns = [
           {chunk_id, timeuuid},   % ID of chunk
           {file_id, timeuuid},    % ID of the file
           {data, blob}            % Data in this chunk
       ],
       primary_key = chunk_id
    };

table_definition(message_archive) ->
    #table_def{
       name = message_archive,
       columns = [
           {id, varint}, % IDs are 64-bit unsigned
           {lower_jid, text},
           {upper_jid, text},
           {time, timeuuid},
           {sent_to_lower, boolean},
           {message, blob}
       ],
       primary_key = [[lower_jid, upper_jid], time],
       order_by = [{time, asc}]
    };

%% Tokens for authenticating individual resources
table_definition(auth_token) ->
    #table_def{
       name = auth_token,
       columns = [
           {user, timeuuid},       % User ID (userpart of JID)
           {server, text},         % Server (domainpart of JID)
           {resource, text},       % Resource (resourcepart of JID)
           {auth_token, text}      % Token
       ],
       primary_key = [user, server, resource]
    }.

table_indexes(session) -> [
    [node]
];
table_indexes(_) -> [].

table_views(user) -> [
    {auth_user, all, [auth_user, user], []}
];
table_views(roster) -> [
    {roster_version, all, [user, version, contact], [{version, asc}]}
];
table_views(session) -> [
    {user_sessions, all, [jid_user, jid_resource, sid], []}
];
table_views(message_archive) -> [
    {archive_id, [id, lower_jid, upper_jid, time],
     [lower_jid, upper_jid, id, time], []}
];

table_views(_) -> [].


%%====================================================================
%% Seed data
%%====================================================================

seed_data(handle_to_user) ->
    [maps:with([user, server, handle, skip], U) || U <- seed_data(user)];
seed_data(phone_number_to_user) ->
    [maps:with([user, server, phone_number, skip], U) || U <- seed_data(user)];
seed_data(user) ->
    Users = [
        #{user => ?ALICE,  handle => ?HANDLE,
          phone_number => ?PHONE_NUMBER, auth_user => ?AUTH_USER},
        #{user => ?CAROL,  handle => <<"carol">>,
          phone_number => <<"+4567">>, auth_user => <<"123456">>},
        #{user => ?BOB,    handle => <<"bob">>,
          phone_number => <<"+8901">>, auth_user => <<"958731">>},
        #{user => ?KAREN,  handle => <<"karen">>,
          phone_number => <<"+5555">>, auth_user => <<"598234">>},
        #{user => ?ROBERT, handle => <<"robert">>,
          phone_number => <<"+6666">>, auth_user => <<"888312">>}
    ],
    [U#{server => ?SERVER, password => ?SCRAM} || U <- Users];
seed_data(last_activity) ->
    Activity = [
        #{user => ?ALICE,  timestamp => 1000, status => <<"Not here">>},
        #{user => ?CAROL,  timestamp => 777,  status => <<"Ennui">>},
        #{user => ?BOB,    timestamp => 666,  status => <<"">>},
        #{user => ?KAREN,  timestamp => 999,  status => <<"Excited">>},
        #{user => ?ROBERT, timestamp => 888,  status => <<"Bored">>}
    ],
    lists:map(
      fun (#{timestamp := TS} = A) ->
          A#{timestamp := wocky_db:seconds_to_timestamp(TS), server => ?SERVER}
      end,
      Activity);
seed_data(session) ->
    lists:flatmap(
      fun (#{user := User, sids := SIDs}) ->
          [make_session(User, SID) || SID <- SIDs]
      end,
      session_sids());
seed_data(offline_msg) ->
    NowSecs = get_nowsecs(),
    lists:flatmap(
      fun (#{user := User, handle := Handle}) ->
          make_offline_msgs(User, Handle, NowSecs, 10)
      end,
      seed_data(user));
seed_data(roster) ->
    Items = [
        #{contact => sjid(?BOB),    nick => <<"bobby">>,  version => 666},
        #{contact => sjid(?CAROL),  nick => <<"carrie">>, version => 777},
        #{contact => sjid(?ROBERT), nick => <<"bob2">>,   version => 888},
        #{contact => sjid(?KAREN),  nick => <<"kk">>,     version => 999}
    ],
    [I#{user => ?ALICE, server => ?SERVER, groups => [<<"friends">>]} ||
        I <- Items];
seed_data(message_archive) ->
    Rows = [random_message(ID, archive_users()) || ID <- lists:seq(1, 300)],
    Q = "INSERT INTO message_archive (id, time, lower_jid, upper_jid,
         sent_to_lower, message) VALUES (?, minTimeuuid(:time), ?, ?, ?, ?)",
    {Q, Rows};
seed_data(auth_token) ->
    [#{user => ?ALICE, server => ?SERVER, resource => ?RESOURCE,
       auth_token => ?TOKEN}];
seed_data(_) ->
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

make_session(User) ->
    make_session(User, fake_sid()).

make_session(User, SID) ->
    #{user => User,
      sid => term_to_binary(SID),
      server => ?SERVER,
      node => node(),
      jid_user => User,
      jid_server => ?SERVER,
      jid_resource => fake_resource(),
      priority => random_priority(),
      info => term_to_binary(session_info())}.

fake_sid() ->
    {fake_now(), fake_pid()}.

fake_now() ->
    list_to_tuple([erlang:unique_integer([positive, monotonic])
                   || _ <- lists:seq(1, 3)]).

fake_pid() ->
    spawn(fun() -> ok end). % Unique(ish) PID

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

make_offline_msgs(User, Handle, NowSecs, N) ->
    [make_offline_msg(User, Handle, NowSecs, I) || I <- lists:seq(1, N)].

make_offline_msg(User, Handle, NowSecs, I) ->
    ExpireSecs = NowSecs + 1000,
    FromJID = jid(<<"from_user">>, <<"from_server">>, <<"res1">>),
    ToJID = jid(<<"to_user">>, <<"to_server">>, <<"res2">>),
    #{user => User,
      server => ?SERVER,
      msg_id => wocky_db_user:create_id(),
      timestamp => wocky_db:seconds_to_timestamp(NowSecs + I),
      expire => wocky_db:seconds_to_timestamp(ExpireSecs),
      from_id => jid:to_binary(FromJID),
      to_id => jid:to_binary(ToJID),
      packet => msg_xml_packet(Handle),
      '[ttl]' => ts_to_ttl(ExpireSecs)}.

sjid(User) ->
    iolist_to_binary([User, "@", ?SERVER]).

jid(User, Server, Resource) ->
    #jid{user = User, server = Server, resource = Resource,
         luser = User, lserver = Server, lresource = Resource}.

msg_xml_packet(Handle) ->
    <<"<message xml:lang=\"en\" type=\"chat\"
                to=\"278e4ba0-b9ae-11e5-a436-080027f70e96@localhost\">
           <body>Hi, ", Handle/binary, "</body>
       </message>">>.

ts_to_ttl(TS) ->
    wocky_db:expire_to_ttl(wocky_db:timestamp_to_now(TS * 1000)).

archive_users() ->
    [<<"bob@localhost">>,
     <<"alice@localhost">>,
     <<"karen@localhost">>
    ].

random_message(ID, Users) ->
    From = lists:nth(rand:uniform(length(Users)), Users),
    RemainingUsers = Users -- [From],
    To = lists:nth(rand:uniform(length(RemainingUsers)), RemainingUsers),
    archive_row(ID, From, To).

archive_row(ID, From, To) ->
    V = mod_wocky_mam:jid_key(From, To),
    V#{id => ID,
       time => ID,
       sent_to_lower => rand:uniform(2) =:= 1,
       message => msg_xml_packet(<<To/binary,
                                   (integer_to_binary(ID))/binary>>)
      }.
