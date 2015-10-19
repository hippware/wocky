%%% @copyright 2015+ Hippware, Inc.
%%% @doc A trial module to store XEP-0012 Last Activity data into Cassandra with seestar
%%%
%%% This module is just for testing and demonstration purposes. 
%%% It is not suitable for production.

%%% Cassandra CQL initialisation 
%%%
%%% USE <keyspace>;
%%%
%%% CREATE TABLE last_activity (
%%%   user varchar,
%%%   server varchar,
%%%   timestamp timestamp,
%%%   status varchar,
%%%   PRIMARY KEY (user, server));
%%%
%%% CREATE INDEX last_activity_server on last_activity(server);
%%% CREATE INDEX last_activity_timestamp ON last_activity(timestamp);

%%% Enable with the following in ejabberd.cfg
%%% {mod_last, [{backend, ca_seestar}, 
%%%             % For protocol_v2
%%%             {auth, {seestar_password_auth, {<<"username">>, <<"password">>}}},      
%%%             {keyspace, "<keyspace>"}
%%%            ]},

-module(mod_last_ca_seestar).

-behaviour(mod_last).

-export([init/2,
         get_last/2,
         count_active_users/3,
         set_last_info/4,
         remove_user/2]).

-include("ejabberd.hrl").
-include("mod_last.hrl").

% Default configuration
-define(DEFAULTS, [
    {address, {"localhost", 9042}}, 
    {keyspace, ?MODULE}
]).

% Using ets to store state for lack of a more convenient cross-process alternative
-define(TABLE, mod_last_ca_seestar).

%% @doc Initialise this module (from {@link mod_last})
-spec init(ejabberd:server(), list()) -> ok.
init(_Host, Opts) ->
    ets:new(?TABLE, [named_table]),

    MergedOpts = lists:ukeymerge(1, lists:ukeysort(1, Opts), lists:ukeysort(1, ?DEFAULTS)),
    {Addr, Port} = gen_mod:get_opt(address, MergedOpts, false),
    {ok, ConnPid} = seestar_session:start_link(Addr, Port, MergedOpts),
    ets:insert(?TABLE, {conn_pid, ConnPid}),

    SelectQuery = "SELECT user, server, timestamp, status FROM last_activity "
        "WHERE user = ? AND server = ?",
    {ok, SelectQueryRes} = seestar_session:prepare(ConnPid, SelectQuery),
    ets:insert(?TABLE, {select_query_id, seestar_result:query_id(SelectQueryRes)}),
    ets:insert(?TABLE, {select_query_types, seestar_result:types(SelectQueryRes)}),

    CountLTQuery = "SELECT COUNT (*) FROM last_activity "
        "WHERE server = ? AND timestamp < ? ALLOW FILTERING",
    {ok, CountLTQueryRes} = seestar_session:prepare(ConnPid, CountLTQuery),
    ets:insert(?TABLE, {count_lt_query_id, seestar_result:query_id(CountLTQueryRes)}),
    ets:insert(?TABLE, {count_lt_query_types, seestar_result:types(CountLTQueryRes)}),

    CountGTQuery = "SELECT COUNT (*) FROM last_activity "
        "WHERE server = ? AND timestamp > ? ALLOW FILTERING",
    {ok, CountGTQueryRes} = seestar_session:prepare(ConnPid, CountGTQuery),
    ets:insert(?TABLE, {count_gt_query_id, seestar_result:query_id(CountGTQueryRes)}),
    ets:insert(?TABLE, {count_gt_query_types, seestar_result:types(CountGTQueryRes)}),

    InsertQuery = "INSERT INTO last_activity "
        "(user, server, timestamp, status) "
        "VALUES (?, ?, ?, ?)",
    {ok, InsertQueryRes} = seestar_session:prepare(ConnPid, InsertQuery),
    ets:insert(?TABLE, {insert_query_id, seestar_result:query_id(InsertQueryRes)}),
    ets:insert(?TABLE, {insert_query_types, seestar_result:types(InsertQueryRes)}),

    DeleteQuery = "DELETE FROM last_activity "
        "WHERE user = ? AND server = ?",
    {ok, DeleteQueryRes} = seestar_session:prepare(ConnPid, DeleteQuery),
    ets:insert(?TABLE, {delete_query_id, seestar_result:query_id(DeleteQueryRes)}),
    ets:insert(?TABLE, {delete_query_types, seestar_result:types(DeleteQueryRes)}),
    ok.

-spec get_last(ejabberd:luser(), ejabberd:lserver()) ->
    {ok, non_neg_integer(), binary()} | {error, term()} | not_found.
get_last(LUser, LServer) ->
    ?INFO_MSG("~p ~p", [LUser, LServer]),

    Args = [iolist_to_binary(LUser), iolist_to_binary(LServer)],
    {ok, Result} = seestar_session:execute(ets:lookup_element(?TABLE, conn_pid, 2), 
                                    ets:lookup_element(?TABLE, select_query_id, 2), 
                                    ets:lookup_element(?TABLE, select_query_types, 2), 
                                    Args, 
                                    one),

    case seestar_result:rows(Result) of
        [] -> 
            not_found;
        [FirstRow | _] -> 
            [_, _, {MegaSeconds, Seconds, _MicroSeconds}, Status] = FirstRow,
            {ok, (MegaSeconds * 1000000 + Seconds), Status}
    end.

-spec count_active_users(ejabberd:lserver(), non_neg_integer(), '<' | '>') ->
    non_neg_integer().
count_active_users(LServer, TimeStamp, Comparator) ->
    ?INFO_MSG("~p ~p ~p", [LServer, TimeStamp, Comparator]),

    {QueryId, QueryTypes} = case Comparator of
        '<' -> 
            {ets:lookup_element(?TABLE, count_lt_query_id, 2), ets:lookup_element(?TABLE, count_lt_query_types, 2)};
        '>' -> 
            {ets:lookup_element(?TABLE, count_gt_query_id, 2), ets:lookup_element(?TABLE, count_gt_query_types, 2)}
    end,

    Args = [iolist_to_binary(LServer), {0, TimeStamp, 0}],
    {ok, Result} = seestar_session:execute(ets:lookup_element(?TABLE, conn_pid, 2), 
                                    QueryId, QueryTypes, Args, one),
    [FirstRow | _] = seestar_result:rows(Result),
    [Count | _] = FirstRow,
    Count.

-spec set_last_info(ejabberd:luser(), ejabberd:lserver(),
                    non_neg_integer(), binary()) ->
    {atomic, ok} | {aborted, term()}.
set_last_info(LUser, LServer, TimeStamp, Status) ->
    ?INFO_MSG("~p ~p ~p ~p", [LUser, LServer, TimeStamp, Status]),

    % Silly: Despite the function spec saying LUser and Status are binaries, 
    % they might also be lists.
    % Consider: seestar should be doing more auto-conversion
    Args = [iolist_to_binary(LUser), iolist_to_binary(LServer), {0, TimeStamp, 0}, iolist_to_binary(Status)],
    {ok, _} = seestar_session:execute(ets:lookup_element(?TABLE, conn_pid, 2), 
                                    ets:lookup_element(?TABLE, insert_query_id, 2), 
                                    ets:lookup_element(?TABLE, insert_query_types, 2), 
                                    Args, 
                                    one),
    {atomic, ok}.

-spec remove_user(ejabberd:luser(), ejabberd:lserver()) -> ok.
remove_user(LUser, LServer) ->
    ?INFO_MSG("~p ~p", [LUser, LServer]),

    Args = [iolist_to_binary(LUser), iolist_to_binary(LServer)],
    {ok, _} = seestar_session:execute(ets:lookup_element(?TABLE, conn_pid, 2), 
                                    ets:lookup_element(?TABLE, delete_query_id, 2), 
                                    ets:lookup_element(?TABLE, delete_query_types, 2), 
                                    Args, 
                                    one),
    ok.
