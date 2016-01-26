-module(wocky_db_seed).

-include("wocky.hrl").
-define(LOCAL_CONTEXT, <<"localhost">>).

-export([create_schema/0, create_schema_for/1, recreate_table/2,
         prepare_table/2, seed_table/2]).


%%====================================================================
%% Helpers
%%====================================================================

create_schema() ->
    create_schema_for(shared),
    create_schema_for(?LOCAL_CONTEXT).

create_schema_for(Context) ->
    ok = wocky_db:create_keyspace(wocky_db:keyspace_name(Context), simple, 1),
    lists:foreach(
      fun (Table) -> ok = recreate_table(Context, Table) end,
      keyspace_tables(Context)).

recreate_table(Context, Name) ->
    ok = wocky_db:drop(Context, table, Name),
    wocky_db:create_table(Context, table_definition(Name)).

prepare_table(Context, Name) ->
    ok = wocky_db:create_keyspace(wocky_db:keyspace_name(Context), simple, 1),
    ok = recreate_table(Context, Name),
    seed_table(Context, Name).

seed_table(Context, Name) ->
    Data = table_data(Name),
    lists:foreach(
      fun (Row) -> {ok, void} = wocky_db:insert_unique(Context, Name, Row) end,
      Data),
    Data.


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
