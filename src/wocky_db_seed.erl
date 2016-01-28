%%% @copyright 2016+ Hippware, Inc.
%%% @doc Helper API for manipulating the database during testing.
%%%
-module(wocky_db_seed).

-include("wocky.hrl").
-include("wocky_db_seed.hrl").

-export([create_schema/0, create_schema_for/1, recreate_table/2,
         create_table_indexes/2, seed_table/2, foreach_table/3, map_tables/3,
         prepare_tables/2, seed_tables/2, clear_tables/2]).


%%====================================================================
%% Helpers
%%====================================================================

create_schema() ->
    create_schema_for(shared),
    create_schema_for(?LOCAL_CONTEXT).

create_schema_for(Context) ->
    prepare_tables(Context, keyspace_tables(wocky_db:keyspace_name(Context))).

recreate_table(Context, Name) ->
    ok = wocky_db:drop(Context, table, Name),
    ok = wocky_db:create_table(Context, table_definition(Name)),
    create_table_indexes(Context, Name).

create_table_indexes(Context, Table) ->
    lists:foreach(
      fun (IdxKeys) -> ok = wocky_db:create_index(Context, Table, IdxKeys) end,
      table_indexes(Table)).

seed_table(Context, Name) ->
    Data = table_data(Name),
    lists:foreach(
      fun (Row) -> true = wocky_db:insert_unique(Context, Name, Row) end,
      Data),
    {ok, Data}.


foreach_table(Context, Fun, Tables) ->
    lists:foreach(fun (Table) -> ok = Fun(Context, Table) end, Tables).

map_tables(Context, Fun, Tables) ->
    lists:map(fun (Table) -> Fun(Context, Table) end, Tables).

prepare_tables(Context, Tables) ->
    ok = wocky_db:create_keyspace(Context, simple, 1),
    ok = foreach_table(Context, fun recreate_table/2, Tables),
    flush_cqerl_prepared_query_cache().

seed_tables(Context, Tables) ->
    map_tables(Context,
               fun (_, Table) ->
                   {ok, Data} = seed_table(Context, Table),
                   {Table, Data}
               end,
               Tables).

clear_tables(Context, Tables) ->
    foreach_table(Context, fun wocky_db:truncate/2, Tables).


%% This is an incredibly ugly hack to work around a problem in cqerl.
%% When we drop tables the C* server discards all cached prepared queries
%% related to that table. cqerl doesn't do this, and it does not handle the
%% error returned from the server that says "this prepared query doesn't exist".
%% TODO: Fix cqerl so that it properly handles the server message indicating
%% that a prepared query no longer exists in the cache. Example:
%% {error, {9472, <<"Prepared query with ID a458ba918d03a5959c2fb8498882cfc5
%% not found (either the query was not prepared on this host (maybe the host
%% has been restarted?) or you have prepared too many queries and it has been
%% evicted from the internal cache)">>,
%% <<164,88,186,145,141,3,165,149,156,47,184,73,136,130,207,197>>}}
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
    roster
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
           {status, bigint},       %
           {user_status, text},    %
           {avatar, timeuuid}      % UUID of file containing user's avatar
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
           {contact, text},        % Simple JID for contact
           {active, boolean},      % True if the roster item is not deleted
           {nick, text},           % Display name for contact chosen by the user
           {groups, {set, text}},  % List of groups the contact belongs to
           {ask, text},            %
           {askmessage, text},     %
           {subscription, text},   %
           {version, timeuuid}     % Timeuuid indicating when the roster item
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
       primary_key = sid
    };

%% Lookup table mapping user ids to a list of session ids
table_definition(user_to_sids) ->
    #table_def{
       name = user_to_sids,
       columns = [
           {jid_user, timeuuid},   % User ID (userpart of JID)
           {sids, {set, blob}}     % List of session IDs
       ],
       primary_key = jid_user
    }.

table_indexes(roster) -> [
    [active],
    [version]
];
table_indexes(session) -> [
    [node]
];
table_indexes(_) -> [].


%%====================================================================
%% Seed data
%%====================================================================

table_data(handle_to_user) -> [
    #{user => ?USER, server => ?SERVER, handle => ?HANDLE}
];
table_data(user) -> [
    #{user => ?USER, server => ?SERVER, handle => ?HANDLE, password => ?PASS}
];
table_data(_) ->
    [].
