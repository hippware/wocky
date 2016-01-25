-module(wocky_db_seed).

-include("wocky.hrl").

-export([recreate_schema/1, recreate_keyspace/1, recreate_table/2]).


%%====================================================================
%% Helpers
%%====================================================================

recreate_schema(Context) ->
    ok = recreate_keyspace(Context),
    lists:foreach(
      fun (Table) -> ok = recreate_table(Context, Table) end,
      keyspace_tables(wocky_db:keyspace_name(Context))).

recreate_keyspace(Context) ->
    ok = wocky_db:drop(Context, keyspace, wocky_db:keyspace_name(Context)),
    wocky_db:create_keyspace(Context, simple, 1).

recreate_table(Context, Name) ->
    ok = wocky_db:drop(Context, table, Name),
    wocky_db:create_table(Context, table_definition(Name)).


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
