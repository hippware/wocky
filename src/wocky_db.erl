%%% @copyright 2015+ Hippware, Inc.
%%% @doc Wocky specific interface to Cassandra
%%%
%%% This module wraps the Cassandra driver providing a simpler interface and
%%% some abstractions that are specific to Wocky.
%%%
%%% All queries are executed within a specific "context". This context can
%%% either be "shared" for data that is used by all instances of Wocky, or
%%% it can be a name specific to this instance (or cluster of instances). The
%%% keyspace name is deduced from the context.
%%%
-module(wocky_db).
-include_lib("cqerl/include/cqerl.hrl").

-type context() :: shared | binary().
-type query() :: iodata().
-type value() :: parameter_val().
-type values() :: [named_parameter()].
-type row() :: values() | maps:map().
-type error() :: term().
-opaque result() :: #cql_result{}.
-export_type([context/0, query/0, value/0, values/0,
              row/0, result/0, error/0]).

%% API
-export([query/3, query/4, batch_query/5, count/2,
         rows/1, single_result/1, single_row/1, to_keyspace/1]).


%%====================================================================
%% API
%%====================================================================

%% @doc Execute a query.
%%
%% A wrapper around {@link query/4}
-spec query(context(), query(), consistency_level())
           -> {ok, void} | {ok, result()} | {error, error()}.
query(Context, Query, Consistency) ->
    query(Context, Query, [], Consistency).

%% @doc Execute a query.
%%
%% `Context' is the context to execute the query in.
%%
%% `Query' is a query string where '?' characters are substituted with
%% parameters from the `Values' list.
%%
%% `Values' is a property list of column name, value pairs. The pairs must be
%% in the same order that the columns are listed in `Query'.
%%
%% On successful completion, the function returns `{ok, void}' when there are
%% no results to return and `{ok, Result}' when there is. `Result' is an
%% abstract datatype that can be passed to {@link rows/1} or
%% {@link single_result/1}.
%%
-spec query(context(), query(), values(), consistency_level())
           -> {ok, void} | {ok, result()} | {error, error()}.
query(Context, Query, Values, Consistency) ->
    run_query(Context, #cql_query{statement = Query,
                                  values = Values,
                                  reusable = true,
                                  consistency = Consistency}).

%% @doc Executes a query multiple times with different datasets in a single
%% batch.
%%
%% `Context' is the context to execute the query in.
%%
%% `Query' is a query string where '?' characters are substituted with
%% parameters from the entries in the `Values' list.
%%
%% `Values' is a list where each element is a property list of column name,
%% value pairs. The pairs must be in the same order that the columns are
%% listed in `Query'.
%%
%% On successful completion, the function returns `{ok, void}'.
%%
-spec batch_query(context(), query(), values(),
                  batch_mode(), consistency_level()
                 ) -> {ok, void} | {error, error()}.
batch_query(Context, Query, Values, Mode, Consistency) ->
    Q = #cql_query{statement = Query},
    BQ = #cql_query_batch{mode = Mode, consistency = Consistency},
    run_query(Context, make_batch_query(Q, Values, BQ)).

%% @doc Extracts the first row from a query result
%%
%% The row is a property list of column name, value pairs.
%%
-spec single_row(result()) -> row() | undefined.
single_row(Result) ->
    case cqerl:head(Result) of
        empty_dataset -> undefined;
        R -> R
    end.

%% @doc Extracts rows from a query result
%%
%% Returns a list of rows. Each row is a property list of column name, value
%% pairs.
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

%% @doc Counts the number of rows in a result that match the predicate.
%%
%% The predicate function must take a row and return a boolean.
%%
-spec count(fun ((values()) -> boolean()), result()) -> non_neg_integer().
count(Pred, Result) ->
    Rows = rows(Result),
    lists:foldl(
      fun(E, Acc) ->
              case Pred(E) of
                  true -> Acc + 1;
                  false -> Acc
              end
      end,
      0,
      Rows).

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

keyspace_name(Context) when is_atom(Context) ->
    keyspace_name(atom_to_list(Context));
keyspace_name(Context) ->
    iolist_to_binary([keyspace_prefix(), Context]).

run_query(Context, Query) ->
    case cqerl:new_client({}, [{keyspace, keyspace_name(Context)}]) of
        {ok, Client} ->
            Return = cqerl:run_query(Client, Query),
            cqerl:close_client(Client),
            Return;
        {closed, Error} ->
            {error, Error}
    end.

make_batch_query(_Q, [], #cql_query_batch{queries = Qs} = Batch) ->
    Batch#cql_query_batch{queries = lists:reverse(Qs)};
make_batch_query(Q, [Vs | Rest], #cql_query_batch{queries = Qs} = Batch) ->
    make_batch_query(Q, Rest,
        Batch#cql_query_batch{queries = [Q#cql_query{values = Vs} | Qs]}).
