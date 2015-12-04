%%% @copyright 2015+ Hippware, Inc.
%%% @doc Backend module which interfaces with the Cassandra driver.
%%%
%%% == Configuration ==
%%%
%%% There are two keyspaces. The "host" keyspace is for host/domain/vhost-specific tables.
%%% The `shared' keyspace is for tables shared across all nodes in all data centers.
%%%
%%% If there are multiple vhosts, there should only be one vhost with one
%%% shared keyspace since the shared keyspace is common across vhosts.
%%%
%%% Each keyspace contains a list of server entries.
%%% Each server entry contains connection (server,port,workers) and credential information (auth).
%%%
%%% Any tuples in the top level (besides `{keyspaces, ...}') is replicated into each keyspace.
%%% Any tuples in the keyspace level (besides `{keyspace, ...}') is replicated into each server entry.
%%% This allows credentials to be set at the server, keyspace, or top level.
%%%
%%% In the keyspace name(s), `%h' will be replaced by the name of the host/domain/vhost,
%%% after replacing punctuation with underscores and truncating to 48 characters.
%%%
%%% ```
%%% {cassandra_seestar, [
%%%     %% Optional common username/password
%%%     {auth, {seestar_password_auth, {<<"common-username">>, <<"common-password">>}}},
%%%     {keyspaces, [
%%%         {host, [
%%%           {keyspace, "prefix_%h"},
%%%           %% Optional keyspace-specific username/password
%%%           {auth, {seestar_password_auth, {<<"keyspace-username">>, <<"keyspace-password">>}}},
%%%           {servers, [
%%%             [{server, "localhost"}, {port, 9042}, {min_workers, 1}, {max_workers, 5},
%%%              %% Optional server-specific username/password
%%%              {auth, {seestar_password_auth, {<<"server-username">>, <<"server-password">>}}}],
%%%             [{server, "localhost"}, {port, 9042}, {min_workers, 1}, {max_workers, 5},
%%%              {auth, {seestar_password_auth, {<<"server-username">>, <<"server-password">>}}}]
%%%           ]}
%%%       ]},
%%%       {shared, [
%%%           {keyspace, "prefix_shared"},
%%%           {auth, {seestar_password_auth, {<<"keyspace-username">>, <<"keyspace-password">>}}},
%%%           {servers, [
%%%             [{server, "localhost"}, {port, 9042}, {min_workers, 2}, {max_workers, 5},
%%%              {auth, {seestar_password_auth, {<<"server-username">>, <<"server-password">>}}}]
%%%           ]}
%%%       ]}
%%%     ]}
%%% ]}.
%%% '''
%%%
%%% To enable, add the following to the modules list in ejabberd.cfg
%%%
%%% ```
%%%   {cassandra, [{backend, seestar}],
%%% '''
%%%
%%% == API ==
%%%
%%% For API documentation, see {@link wocky_db}

-module(wocky_db_seestar).

-include_lib("seestar/include/constants.hrl").

%% gen_server
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/2]).

%% Interface functions
-behaviour(wocky_db_backend).
-export([configure/2, clear/0,
         aquery/5,
         pquery/5, pquery_async/5,
         batch_pquery/4,
         rows/1]).

% Default configuration
-define(DEFAULT_MIN_WORKERS, 2).
-define(DEFAULT_MAX_WORKERS, 5).
-define(DEFAULT_SERVERS, [[{server, "localhost"},
                           {port, 9042},
                           {min_workers, ?DEFAULT_MIN_WORKERS},
                           {max_workers, ?DEFAULT_MAX_WORKERS}]]).

-record(state, {
    host,
    conn,
    pqueries,
    async_query_refs}).

-record(pquery, {query, id, types}).


%%====================================================================
%% Interface functions
%%====================================================================

configure(Host, Config) ->
    {HostServers, [SharedServers]} = prepare_config(Host, Config),
    {ok, _} = create_worker_pool(shared, SharedServers),
    lists:foreach(fun(HostServer) ->
                          {ok, _} = create_worker_pool(Host, HostServer)
                  end, HostServers).

clear() ->
    delete_worker_pools().

aquery(Host, Query, Values, Consistency, PageSize) ->
    call_worker(Host, {adhoc_query, Query, Values, Consistency, PageSize}).

pquery(Host, Query, Values, Consistency, PageSize) ->
    call_worker(Host, {prepared_query, Query, Values, Consistency, PageSize}).

% ToDo:
% Needs review to ensure the function preconditions are suitable.
% Maybe should be a gen_server:cast() instead?
pquery_async(Host, Query, Values, Consistency, PageSize) ->
    call_worker(Host, {prepared_query_async, Query, Values, Consistency, PageSize}).

batch_pquery(Host, Queries, Type, Consistency) ->
    call_worker(Host, {batch_prepared_queries, Queries, Type, Consistency}).

rows(Result) ->
    seestar_result:rows(Result).

%%====================================================================
%% gen_server callbacks
%%====================================================================

start_link(Host, Server) ->
    gen_server:start_link(?MODULE, [Host, Server], []).

init([Host, ServerSettings]) ->
    lager:info("Seestar worker starting up for ~p", [Host]),
    Server = proplists:get_value(server, ServerSettings),
    Port = proplists:get_value(port, ServerSettings),
    {ok, ConnPid} = seestar_session:start_link(Server, Port, ServerSettings),

    State = #state{host=Host,
                   conn=ConnPid,
                   pqueries=#{},
                   async_query_refs=dict:new()},
    {ok, State}.

handle_call({adhoc_query, Query, Values, Consistency, PageSize}, _From, State=#state{conn=ConnPid}) ->
    Result = seestar_session:perform(ConnPid, Query, Consistency, Values, PageSize),
    {reply, Result, State};

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
            lager:warning("Unprepared query ~p: Retrying (~p)", [Query, Retry]),
            handle_call(Request, From, remove_prepared_query(Query, State), Retry - 1);
        {error, {error, ?UNPREPARED, _, _}} ->
            lager:error("Unprepared query ~p: Terminated", [Query]),
            {reply, Result, NewState};
        _ ->
            {reply, Result, NewState}
    end.

handle_cast(Msg, State) ->
    lager:warning("Unknown cast message ~p.", [Msg]),
    {noreply, State}.

handle_info({seestar_response, QueryRef, ResultFunc}, State) ->
    {noreply, forward_async_query_response(ResultFunc, QueryRef, State)};
handle_info(Msg, State) ->
    lager:warning("Unknown info message ~p.", [Msg]),
    {noreply, State}.

terminate(_Reason, _State=#state{conn=ConnPid}) ->
    seestar_session:stop(ConnPid).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

%% Return {HostServers, SharedServers} : A list of server connection details
%% for the host keyspace, and a list of the same for the shared keyspace
prepare_config(Host, Config) ->
    %% Merge top level properties into keyspace properties
    {value, {keyspaces, Keyspaces0}, TopProperties} = lists:keytake(keyspaces, 1, Config),
    Keyspaces = lists:map(
                  fun ({KeyspaceType, KeyspaceProperties}) ->
                          Properties =
                              lists:ukeymerge(1,
                                              lists:ukeysort(1, KeyspaceProperties),
                                              lists:ukeysort(1, TopProperties)),

                          %% Also extract keyspace name and ...
                          {keyspace, Name0} = lists:keyfind(keyspace, 1, Properties),
                          %% ... replace %h with Host
                          Name1 = re:replace(Name0, "%h", Host, [global]),
                          KeyspaceName = wocky_db:to_keyspace(Name1),

                          {KeyspaceType,
                           lists:keyreplace(keyspace, 1, Properties, {keyspace, KeyspaceName})}
                  end, Keyspaces0),

    %% Merge keyspace properties into servers list
    Result = lists:foldl(
               fun ({KeyspaceType, Opts}, {HostServers, SharedServers}) ->
                       {Properties, Servers} =
                           case lists:keytake(servers, 1, Opts) of
                               false ->
                                   {Opts, ?DEFAULT_SERVERS};

                               {value, Tuple, TupleList} ->
                                   {servers, Value} = Tuple,
                                   {TupleList, Value}
                           end,
                       NewServers = lists:map(
                                      fun(A) ->
                                              lists:ukeymerge(1,
                                                              lists:ukeysort(1, A),
                                                              lists:ukeysort(1, Properties))
                                      end, Servers),

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

create_worker_pool(_Name, []) ->
    {ok, empty};
create_worker_pool(Name, Config) when is_list(Name) ->
    create_worker_pool(list_to_atom(Name), Config);
create_worker_pool(Name, Config) when is_binary(Name) ->
    create_worker_pool(binary_to_atom(Name, utf8), Config);
create_worker_pool(Name, Config) when is_atom(Name) ->
    %% Ensure that there isn't an old pool with the same name
    ok = pooler:rm_pool(Name),

    MinWorkers = proplists:get_value(min_workers, Config, ?DEFAULT_MIN_WORKERS),
    MaxWorkers = proplists:get_value(max_workers, Config, ?DEFAULT_MAX_WORKERS),
    PoolConfig = [{name, Name},
                  {group, seestar},
                  {init_count, MinWorkers},
                  {max_count, MaxWorkers},
                  {start_mfa,
                   {wocky_db_seestar, start_link, [Name, Config]}}],

    pooler:new_pool(PoolConfig).

delete_worker_pools() ->
    pooler:rm_group(seestar).

call_worker(Host, Request) when is_list(Host) ->
    Name = list_to_existing_atom(Host),
    call_worker(Name, Request);
call_worker(Host, Request) when is_binary(Host) ->
    Name = binary_to_existing_atom(Host, utf8),
    call_worker(Name, Request);
call_worker(Host, Request) when is_atom(Host) ->
    Pid = pooler:take_member(Host),
    try
        gen_server:call(Pid, Request)
    after
        pooler:return_member(Host, Pid, ok)
    end.

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
            lager:warning("Dropping async response ~p ~p", [QueryRef, ResultFunc()]),
            State
    end.
