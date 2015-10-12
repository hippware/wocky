%%%----------------------------------------------------------------------
%%% File    : cassandra_seestar.erl
%%% Author  : Beng Tan
%%% Purpose : Backend module which interfaces with the Cassandra driver.
%%%
%%% Copyright (C) 2015 Hippware
%%%----------------------------------------------------------------------

%%% Configuration
%%% 
%%% There are two keyspaces. The "host" keyspace is for host/domain/vhost-specific tables.
%%% The "shared" keyspace is for tables shared across all nodes in all data centers.
%%% 
%%% Each keyspace contains a list of server entries. 
%%% Each server entry contains connection (server,port,workers) and credential information (auth).
%%% 
%%% Any tuples in the top level (besides {keyspaces, ...}) is replicated into each keyspace.
%%% Any tuples in the keyspace level (besides {keyspace, ...}) is replicated into each server entry.
%%% This allows credentials to be set at the server, keyspace, or top level.
%%% 
%%% In the keyspace name(s), %h will be replaced by the name of the host/domain/vhost, 
%%% after replacing punctuation with underscores and truncating to 48 characters.
%%% 
%%% {cassandra_seestar, [
%%%     % Optional common username/password
%%%     {auth, {seestar_password_auth, {<<"common-username">>, <<"common-password">>}}},
%%%     {keyspaces, [
%%%         {host, [
%%% 	        {keyspace, "prefix_%h"},
%%% 	        % Optional keyspace-specific username/password
%%% 	        {auth, {seestar_password_auth, {<<"keyspace-username">>, <<"keyspace-password">>}}},
%%% 	        {servers, [
%%% 		        [{server, "localhost"}, {port, 9042}, {workers, 1}, 
%%% 		         % Optional server-specific username/password
%%% 		         {auth, {seestar_password_auth, {<<"server-username">>, <<"server-password">>}}}],
%%% 		        [{server, "localhost"}, {port, 9042}, {workers, 1}, 
%%% 		         {auth, {seestar_password_auth, {<<"server-username">>, <<"server-password">>}}}]
%%% 	        ]}	
%%% 	    ]},
%%% 	    {shared, [
%%% 	        {keyspace, "prefix_shared"},
%%% 	        {auth, {seestar_password_auth, {<<"keyspace-username">>, <<"keyspace-password">>}}},
%%% 	        {servers, [
%%% 		        [{server, "localhost"}, {port, 9042}, {workers, 2}, 
%%% 		         {auth, {seestar_password_auth, {<<"server-username">>, <<"server-password">>}}}]
%%% 	        ]}	
%%% 	    ]}
%%%     ]}
%%% ]}.

%%% To enable, add the following to the modules list in ejabberd.cfg
%%% 
%%%   {cassandra, [{backend, seestar}],

-module(cassandra_seestar).
-include("ejabberd.hrl").

-include_lib("seestar/include/constants.hrl").

%% gen_mod
-behaviour(gen_mod).
-export([start/2, stop/1]).

%% gen_server
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Interface functions
-behaviour(cassandra_gen_backend).
-export([start_link/2,
         aquery/5, 
         prepare_query/2, pquery/5, pquery_async/5,
         batch_pquery/4,
         rows/1]).

-record(state, {
    host,
    conn, 
    pqueries, 
    async_query_refs}).

-record(pquery, {query, id, types}).

% Default configuration
-define(DEFAULT_SERVERS, [[{server, "localhost"}, {port, 9042}, {workers, 1}]]).

%% gen_mod callbacks
start(Host, Opts) ->
    {HostServers, SharedServers} = get_servers(Host),

    if 
        length(SharedServers) > 0 ->
            create_worker_pool(shared),
            cassandra_seestar_sup:start(shared, SharedServers);
        true ->
            ok
    end,

    create_worker_pool(Host),
    cassandra_seestar_sup:start(Host, HostServers).

%% Return {HostServers, SharedServers} : A list of server connection details for the host keyspace, and a list of the same for the shared keyspace
get_servers(Host) ->
    Config = ejabberd_config:get_local_option({?MODULE, Host}),

    % Merge top level properties into keyspace properties
    {value, {keyspaces, Keyspaces0}, TopProperties} = lists:keytake(keyspaces, 1, Config),
    Keyspaces = lists:map(fun ({KeyspaceType, KeyspaceProperties}) -> 
                        Properties = lists:ukeymerge(1, lists:ukeysort(1,KeyspaceProperties), lists:ukeysort(1, TopProperties)),

                        % Also extract keyspace name and ...
                        {keyspace, Name0} = lists:keyfind(keyspace, 1, Properties),
                        % ... replace %h with Host
                        Name1 = re:replace(Name0, "%h", Host, [global]),
                        {KeyspaceType, lists:keyreplace(keyspace, 1, Properties, {keyspace, cassandra:to_keyspace(Name1)})}
                     end, Keyspaces0),

    % Merge keyspace properties into servers list
    Result = lists:foldl(fun ({KeyspaceType, Opts}, {HostServers, SharedServers}) -> 
                    {Properties, Servers} = case lists:keytake(servers, 1, Opts) of
                        false ->
                            {Opts, ?DEFAULT_SERVERS};
                        {value, Tuple, TupleList} ->
                            {servers, Value} = Tuple,
                            {TupleList, Value}
                    end,
                    NewServers = lists:map(fun(A) -> lists:ukeymerge(1, lists:ukeysort(1, A), lists:ukeysort(1, Properties)) end, Servers),

                    case KeyspaceType of 
                        host -> 
                            {NewServers, SharedServers};
                        shared ->
                            {HostServers, NewServers};
                        _ ->
                            {HostServers, SharedServers}
                    end                    
                end, {[], []}, Keyspaces),
    Result.

stop(Host) ->
    {_, SharedServers} = get_servers(Host),
    if 
        length(SharedServers) > 0 ->
            delete_worker_pool(shared),
            cassandra_seestar_sup:stop(shared);
        true ->
            ok
    end,
    delete_worker_pool(Host),
    cassandra_seestar_sup:stop(Host).

%%====================================================================
%% Internal functions
%%====================================================================

create_worker_pool(Host) ->
    pg2:create(group_name(Host)).

delete_worker_pool(Host) ->
    pg2:delete(group_name(Host)).

register_worker(Host, WorkerPid) ->
    pg2:join(group_name(Host), WorkerPid).

select_worker(Host, Query) ->
    case pg2:get_local_members(group_name(Host)) of
        [] ->
            error({no_worker, Host});
        Workers ->
            N = erlang:phash2(Query, length(Workers)) + 1,
            lists:nth(N, Workers)
    end.

group_name(Host) ->
    {?MODULE, Host}.

%%====================================================================
%% Interface functions
%%====================================================================

start_link(Host, Server) ->
    gen_server:start_link(?MODULE, [Host, Server], []).

aquery(Host, Query, Values, Consistency, PageSize) ->
    gen_server:call(select_worker(Host, Query), 
                    {adhoc_query, Query, Values, Consistency, PageSize}).

prepare_query(Host, Query) ->
    gen_server:call(select_worker(Host, Query), 
                    {prepare_query, Query}).

pquery(Host, Query, Values, Consistency, PageSize) ->
    gen_server:call(select_worker(Host, Query), 
                    {prepared_query, Query, Values, Consistency, PageSize}).

% ToDo:
% Needs review to ensure the function preconditions are suitable. 
% Maybe should be a gen_server:cast() instead?
pquery_async(Host, Query, Values, Consistency, PageSize) ->
    gen_server:call(select_worker(Host, Query), 
                    {prepared_query_async, Query, Values, Consistency, PageSize}).

batch_pquery(Host, Queries, Type, Consistency) ->
    gen_server:call(select_worker(Host, Queries), 
                    {batch_prepared_queries, Queries, Type, Consistency}).

rows(Result) ->
    seestar_result:rows(Result).

%%====================================================================
%% Other internal functions
%%====================================================================

add_prepared_query(Query, State=#state{conn=ConnPid, pqueries=PQueries}) ->
    case maps:find(Query, PQueries) of
        {ok, Value} -> 
            {Value, State};
        error ->
            {ok, QueryRes} = seestar_session:prepare(ConnPid, Query),
            NewQuery = #pquery{query = Query, id = seestar_result:query_id(QueryRes), types = seestar_result:types(QueryRes)},
            Key = Query,
            {NewQuery, State#state{pqueries = maps:put(Key, NewQuery, PQueries)}}
    end.

remove_prepared_query(Query, State=#state{pqueries=PQueries}) ->
    State#state{pqueries = maps:remove(Query, PQueries)}.

% Save an async query ref
save_async_query_ref(From, QueryRef, State=#state{async_query_refs=Refs}) ->
    State#state{async_query_refs=dict:store(QueryRef, From, Refs)}.

forward_async_query_response(ResultFunc, QueryRef, State=#state{async_query_refs=Refs}) ->
    case dict:find(QueryRef, Refs) of
        {ok, From} ->
            Refs2 = dict:erase(QueryRef, Refs),
            gen_server:reply(From, ResultFunc),
            State#state{async_query_refs=Refs2};
        error ->
            ?WARNING_MSG("Dropping async response ~p ~p", [QueryRef, ResultFunc()]),
            State
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Host, ServerSettings]) ->
    register_worker(Host, self()),
    {ok, ConnPid} = seestar_session:start_link(proplists:get_value(server, ServerSettings), proplists:get_value(port, ServerSettings), ServerSettings),

    State = #state{
        host=Host,
        conn=ConnPid,
        pqueries=#{}, 
        async_query_refs=dict:new()},
    {ok, State}.

handle_call({adhoc_query, Query, Values, Consistency, PageSize}, _From, State=#state{conn=ConnPid}) ->
    Result = seestar_session:perform(ConnPid, Query, Consistency, Values, PageSize),
    {reply, Result, State};

handle_call({prepare_query, Query}, _From, State) ->
    {_P, NewState} = add_prepared_query(Query, State),
    {reply, ok, NewState};

handle_call(Request={prepared_query, _Query, _Values, _Consistency, _PageSize}, From, State) ->
    handle_call(Request, From, State, 3);

handle_call({batch_prepared_queries, Queries, Type, Consistency}, _From, State=#state{conn=ConnPid}) ->
    {ReversedQueries, NewState} = lists:foldl(
                fun ({Query, Values}, {Acc, State0}) -> 
                    {P, State1} = add_prepared_query(Query, State0),
                    {[seestar_batch:prepared_query(P#pquery.id, P#pquery.types, Values) | Acc], State1}
                end, {[], State}, Queries),
    Batch = seestar_batch:batch_request(Type, Consistency, lists:reverse(ReversedQueries)),
    Result = seestar_session:batch(ConnPid, Batch),
    {reply, Result, NewState};

handle_call({prepared_query_async, Query, Values, Consistency, PageSize}, From, State=#state{conn=ConnPid}) ->
    {P, NewState} = add_prepared_query(Query, State),
    QueryRef = seestar_session:execute_async(ConnPid, 
                                    P#pquery.id, 
                                    P#pquery.types, 
                                    Values, 
                                    Consistency, PageSize),
    {noreply, save_async_query_ref(From, QueryRef, NewState)}.

handle_call(Request={prepared_query, Query, Values, Consistency, PageSize}, From, State=#state{conn=ConnPid}, Retry) ->
    {P, NewState} = add_prepared_query(Query, State),
    Result = seestar_session:execute(ConnPid, 
                                    P#pquery.id, 
                                    P#pquery.types, 
                                    Values, 
                                    Consistency, PageSize),
    case Result of 
        % If the query is unavailable (ie. schema changed), then forget it and retry
        {error, {error, ?UNPREPARED, _, _}} when Retry > 0 ->
            ?WARNING_MSG("Unprepared query ~p: Retrying (~p)", [Query, Retry]),
            handle_call(Request, From, remove_prepared_query(Query, State), Retry - 1);
        {error, {error, ?UNPREPARED, _, _}} ->
            ?ERROR_MSG("Unprepared query ~p: Terminated", [Query]),
            {reply, Result, NewState};
        _ -> 
            {reply, Result, NewState}
    end.

handle_cast(Msg, State) ->
    ?WARNING_MSG("Unknown cast message ~p.", [Msg]),
    {noreply, State}.

handle_info({seestar_response, QueryRef, ResultFunc}, State) ->
    {noreply, forward_async_query_response(ResultFunc, QueryRef, State)};
handle_info(Msg, State) ->
    ?WARNING_MSG("Unknown info message ~p.", [Msg]),
    {noreply, State}.

terminate(_Reason, _State=#state{conn=ConnPid}) ->
    seestar_session:stop(ConnPid).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
