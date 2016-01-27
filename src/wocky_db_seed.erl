%%% @copyright 2016+ Hippware, Inc.
%%% @doc Helper API for manipulating the database during testing.
%%%
-module(wocky_db_seed).

-include("wocky.hrl").
-define(LOCAL_CONTEXT, <<"localhost">>).

-export([create_schema/0, create_schema_for/1, recreate_table/2, seed_table/2,
         foreach_table/3, map_tables/3, prepare_tables/2, seed_tables/2,
         clear_tables/2]).


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
    wocky_db:create_table(Context, table_definition(Name)).

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
table_definition(user) ->
    #table_def{
       name = user,
       columns = [
           {user, timeuuid},
           {server, text},
           {handle, text},
           {password, text},
           {phone_number, text},
           {status, bigint},
           {user_status, text},
           {avatar, timeuuid}
       ],
       primary_key = user
    };
table_definition(last_activity) ->
    #table_def{
       name = last_activity,
       columns = [
           {user, timeuuid},
           {server, text},
           {timestamp, timestamp},
           {status, text}
       ],
       primary_key = user
    };
table_definition(offline_msg) ->
    #table_def{
       name = offline_msg,
       columns = [
           {user, timeuuid},
           {server, text},
           {msg_id, timeuuid},
           {timestamp, timestamp},
           {expire, timestamp},
           {from_id, text},
           {to_id, text},
           {packet, text}
       ],
       primary_key = [user, timestamp, msg_id],
       order_by = [{timestamp, asc}]
    };
table_definition(roster) ->
    #table_def{
       name = roster,
       columns = [
           {user, timeuuid},
           {server, text},
           {contact, text},
           {active, boolean},
           {nick, text},
           {groups, {set, text}},
           {ask, text},
           {askmessage, text},
           {subscription, text},
           {version, timeuuid}
       ],
       primary_key = [user, contact]
    }.


%%====================================================================
%% Seed data
%%====================================================================

table_data(_) ->
    [].
