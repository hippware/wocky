%%% @copyright 2015+ Hippware, Inc.
%%% @doc Cassandra dispatcher
%%%
%%% This dispatches to the Cassandra driver.
%%%
%%% Whilst this is supposed to be a generic Cassandra interface, for practical
%%% purposes, it is still closely coupled to the backend. If we ever support
%%% more than one backend, then the interface needs to be made backend agnostic.
%%%
%%% The query functions are spec-ced to return `{ok, result()}' but usually
%%% return `{ok, rows_result()}'. `result()' is a generalisation/superset of
%%% `rows_result()'.

-module(wocky_db).
-include_lib("cqerl/include/cqerl.hrl").

-type host() :: binary().
-type query() :: iodata().
-type value() :: cqerl:parameter_val().
-type values() :: [value()].
-type consistency() :: cqerl:consistency_level().
-type batch_mode() :: logged | unlogged | counter.
-type result() :: #cql_result{}.
-type error() :: term().
-export_type([host/0, query/0, value/0, values/0, consistency/0, batch_mode/0]).
-export_type([result/0, error/0]).

%% API
-export([query/3, query/4, query/5, batch_query/5,
         rows/1, single_result/1,
         to_keyspace/1]).


%%====================================================================
%% API
%%====================================================================

%% @doc A wrapper around {@link query/5}
%% @spec query(Host, Query, Consistency) -> {ok, Result :: result()} | {error, Error :: error()}
query(Host, Query, Consistency) ->
    query(Host, Query, [], Consistency, undefined).

%% @doc A wrapper around {@link query/5}
%% @spec query(Host, Query, Values, Consistency) -> {ok, Result :: result()} | {error, Error :: error()}
query(Host, Query, Values, Consistency) when is_list(Values) ->
    query(Host, Query, Values, Consistency, undefined);

query(Host, Query, Consistency, PageSize) when is_atom(PageSize) ->
    query(Host, Query, [], Consistency, PageSize).

%% @doc Execute a query as a prepared query (in the context of a virtual host).
%%
%% `Query' is a query string where '?' characters are substituted with
%% parameters from the `Values' list.
%% `Host' is the virtual host to execute the query for.
%%
-spec query(Host, Query, Values, Consistency, PageSize) -> {ok, Result :: result()} | {error, Error :: error()} when
             Host :: binary(), % ejabberd:server()
             Query :: binary() | string(), % seestar_session:'query'()
             Values :: [value()],
             Consistency :: consistency(),
             PageSize :: non_neg_integer() | undefined.
query(Host, Query, Values, Consistency, PageSize) ->
    run_query(Host, #cql_query{statement = Query,
                               values = Values,
                               reusable = true,
                               consistency = Consistency,
                               page_size = PageSize}).

%% @doc Executes a batch of queries as prepared queries (in the context of a
%% virtual host).
%%
%% `Queries' is a list of `{Query, Values}' tuples where
%% `Query' is a query string where '?' characters are substituted with
%% parameters from the list `Values'.
%%
-spec batch_query(host(), query(), values(), batch_mode(), consistency())
                 -> {ok, void} | {error, error()}.
batch_query(Host, Query, Values, Mode, Consistency) ->
    Q = #cql_query{statement = Query},
    BQ = #cql_query_batch{mode = Mode, consistency = Consistency},
    {ok, void} = run_query(Host, make_batch_query(Q, Values, BQ)).


%% @doc Extracts rows from a query result
%%
%% Returns a list of rows. Each row is a list of values.
%%
-spec rows(result()) -> [values()].
rows(Result) ->
    cqerl:all_rows(Result).

%% @doc Extracts the value of the first column of the first row from a query
%% result
%%
-spec single_result(result()) -> value() | undefined.
single_result(Result) ->
    case cqerl:head(Result) of
        empty_dataset -> undefined;
        [{_Name, Value}|_] -> Value
    end.

%% @doc Modify a string so it is a valid keyspace
%%
%% All invalid characters are replaced with underscore and then
%% truncated to 48 characters. Returns the modified string.
%%
-spec to_keyspace(binary()) -> binary().
to_keyspace(String) ->
    Space = iolist_to_binary(re:replace(String, "[^0-9a-z]", "_", [global])),
    case byte_size(Space) of
        Size when Size > 48 ->
            {Head, _} = split_binary(Space, 48),
            Head;
        _ ->
            Space
    end.


%%====================================================================
%% Internal functions
%%====================================================================

keyspace_prefix() ->
    application:get_env(wocky, keyspace_prefix, "wocky_").

keyspace_name(KS) when is_atom(KS) ->
    keyspace_name(atom_to_list(KS));
keyspace_name(KS) ->
    iolist_to_binary([keyspace_prefix(), KS]).

run_query(Keyspace, Query) ->
    {ok, Client} = cqerl:new_client({}, [{keyspace, keyspace_name(Keyspace)}]),
    {ok, Result} = cqerl:run_query(Client, Query),
    cqerl:close_client(Client),
    {ok, Result}.

make_batch_query(_Q, [], #cql_query_batch{queries = Qs} = Batch) ->
    Batch#cql_query_batch{queries = lists:reverse(Qs)};
make_batch_query(Q, [Vs | Rest], #cql_query_batch{queries = Qs} = Batch) ->
    make_batch_query(Q, Rest,
        Batch#cql_query_batch{queries = [Q#cql_query{values = Vs} | Qs]}).
