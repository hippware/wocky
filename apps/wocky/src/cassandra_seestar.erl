%%%----------------------------------------------------------------------
%%% File    : cassandra_seestar.erl
%%% Author  : Beng Tan
%%% Purpose : Backend module which interfaces with the Cassandra driver.
%%%
%%% Copyright (C) 2015 Hippware
%%%----------------------------------------------------------------------

%%% Enable with the following in ejabberd.cfg (preferably first)
%%%
%%% {cassandra, [
%%%     {backend, seestar},
%%%     % common settings which are inserted into servers list
%%%     {keyspace, "<keyspace>"}, 
%%%     {auth, {seestar_password_auth, {<<"username">>, <<"password">>}}}, % For protocol_v2
%%%     % An optional list of servers. 
%%%     % Each server entry inherits common settings from the top level
%%%     {servers, [
%%%         [{server, "localhost"}, {port, 9042}, {workers, 1}]
%%%     ]}
%%%     ]},

-module(cassandra_seestar).
-include("ejabberd.hrl").

%% gen_mod
-behaviour(gen_mod).
-export([start/2, stop/1]).

%% gen_server
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Interface functions
-export([start_link/2,
         aquery/5, 
         pquery/5, pquery_async/5]).

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
    create_worker_pool(Host),
    cassandra_seestar_sup:start(Host, get_servers(Opts)).

get_servers(Opts) ->
    {Properties, Servers} = case lists:keytake(servers, 1, Opts) of
        false ->
            {Opts, ?DEFAULT_SERVERS};
        {value, Tuple, TupleList} ->
            {servers, Value} = Tuple,
            {TupleList, Value}
    end,
    % Merge top level properties into servers list
    lists:map(fun(A) -> lists:ukeymerge(1, lists:ukeysort(1, A), lists:ukeysort(1, Properties)) end, Servers).

stop(Host) ->
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

pquery(Host, Query, Values, Consistency, PageSize) ->
    gen_server:call(select_worker(Host, Query), 
                    {prepared_query, Query, Values, Consistency, PageSize}).

pquery_async(Host, Query, Values, Consistency, PageSize) ->
    gen_server:call(select_worker(Host, Query), 
                    {prepared_query_async, Query, Values, Consistency, PageSize}).

%%====================================================================
%% Other internal functions
%%====================================================================

prepare_query(Query, State=#state{conn=ConnPid, pqueries=PQueries}) ->
    case maps:find(Query, PQueries) of
        {ok, Value} -> 
            {Value, State};
        error ->
            {ok, QueryRes} = seestar_session:prepare(ConnPid, Query),
            NewQuery = #pquery{query = Query, id = seestar_result:query_id(QueryRes), types = seestar_result:types(QueryRes)},
            Key = Query,
            {NewQuery, State#state{pqueries = maps:put(Key, NewQuery, PQueries)}}
    end.

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

init([Host, Server]) ->
    register_worker(Host, self()),
    {ok, ConnPid} = seestar_session:start_link(proplists:get_value(server, Server), proplists:get_value(port, Server), Server),

    State = #state{
        host=Host,
        conn=ConnPid,
        pqueries=#{}, 
        async_query_refs=dict:new()},
    {ok, State}.

handle_call({adhoc_query, Query, Values, Consistency, PageSize}, _From, State=#state{conn=ConnPid}) ->
    {ok, Result} = seestar_session:perform(ConnPid, Query, Consistency, Values, PageSize),
    {reply, Result, State};

handle_call({prepared_query, Query, Values, Consistency, PageSize}, _From, State=#state{conn=ConnPid}) ->
    {P, NewState} = prepare_query(Query, State),
    {ok, Result} = seestar_session:execute(ConnPid, 
                                    P#pquery.id, 
                                    P#pquery.types, 
                                    Values, 
                                    Consistency, PageSize),
    {reply, Result, NewState};

handle_call({prepared_query_async, Query, Values, Consistency, PageSize}, From, State=#state{conn=ConnPid}) ->
    {P, NewState} = prepare_query(Query, State),
    QueryRef = seestar_session:execute_async(ConnPid, 
                                    P#pquery.id, 
                                    P#pquery.types, 
                                    Values, 
                                    Consistency, PageSize),
    {noreply, save_async_query_ref(From, QueryRef, NewState)}.

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
