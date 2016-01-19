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

-type context()  :: shared | binary().
-type query()    :: iodata().
-type value()    :: parameter_val().
-type values()   :: maps:map().
-type row()      :: maps:map().
-type rows()     :: [row()].
-type error()    :: term().
-opaque result() :: #cql_result{}.
-export_type([context/0, query/0, value/0, values/0,
              row/0, result/0, error/0]).

%% API
-export([query/3, query/4, batch_query/4, multi_query/3, multi_query/4,
         rows/1, single_row/1, single_result/1, count/2,
         to_keyspace/1, seconds_to_timestamp/1, timestamp_to_seconds/1,
         timestamp_to_now/1, now_to_timestamp/1, expire_to_ttl/1]).


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
    run_query(Context, make_query(Query, Values, Consistency)).

%% @doc Executes a batch query.
%%
%% In a batch query, multiple queries are executed atomically, but not in
%% isolation. Note that it is a bad idea to update multiple rows on the same
%% table with a batch for performance reasons. This will likely result in worse
%% performance. Executing the same query multiple times with different data sets
%% performs best when normal prepared queries are used (see
%% {@link multi_query/4})
%%
%% This feature is meant for updating multiple tables containing the same
%% denormalized data atomically.
%%
%% `Context' is the context to execute the query in.
%%
%% `Queries' is a list of query/values pairs. The first element is a query
%% string where '?' characters are substituted with parameters from the entries
%% in the values list. The values list is a property list of column name,
%% value pairs. The pairs must be in the same order that the columns are
%% listed in the query.
%%
%% `Mode' is one of the values `logged' (default), `counter' or `unlogged'
%% (deprecated). Use `logged' unless all of the queries update counter columns,
%% then use `counter'.
%%
%% On successful completion, the function returns `{ok, void}'.
%%
-spec batch_query(context(), [{query(), values()}],
                  batch_mode(), consistency_level()
                 ) -> {ok, void} | {error, error()}.
batch_query(Context, QueryList, Mode, Consistency) ->
    run_query(Context, make_batch_query(QueryList, Consistency, Mode)).

%% @doc Executes a query multiple times with different datasets.
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
%% Returns `ok'.
-spec multi_query(context(), query(), [values()], consistency_level()) -> ok.
multi_query(Context, Query, ValuesList, Consistency) ->
    lists:foreach(fun (Values) ->
                          {ok, void} = run_query(Context,
                               make_query(Query, Values, Consistency))
                  end, ValuesList),
    ok.

%% @doc Executes multipe queries with different datasets.
%%
%% This is functionally similar to {@link batch_query/4}, however this
%% function executes the queries individually rather than using C*'s batching
%% system. This is more appropriate (and performant) when operating over
%% multiple bits of data on a single table requiring varied query strings. An
%% example may be where certain rows of data require a TTL setting but others do
%% not. Don't be afraid to use this form if the query strings may be all
%% identical - in this case it will perform exactly the same as {@link
%% multi_query/4} (with the relatively small exception of the extra cost of
%% passing multiple copies of the same query string).
%%
%% `Context' is the context to execute the query in.
%%
%% `QueryVals' is a list of tuples `{Query, Values}' where `Query' and `Values'
%% are as for {@link query/4}.
%%
%% Returns `ok'.
-spec multi_query(context(), [{query(), values()}], consistency_level()) -> ok.
multi_query(Context, QueryVals, Consistency) ->
    lists:foreach(fun ({Query, Values}) ->
                          {ok, void} = run_query(Context,
                               make_query(Query, Values, Consistency))
                  end, QueryVals),
    ok.

%% @doc Extracts rows from a query result
%%
%% Returns a list of rows. Each row is a property list of column name, value
%% pairs.
%%
-spec rows(result()) -> rows().
rows(Result) ->
    drop_all_nulls(cqerl:all_rows(Result)).

%% @doc Extracts the first row from a query result
%%
%% The row is a property list of column name, value pairs.
%%
-spec single_row(result()) -> row().
single_row(Result) ->
    case cqerl:head(Result) of
        empty_dataset -> #{};
        R -> drop_nulls(R)
    end.

%% @doc Extracts the value of the first column of the first row from a query
%% result
%%
-spec single_result(result()) -> value() | undefined.
single_result(Result) ->
    case cqerl:head(Result) of
        empty_dataset ->
            undefined;
        Map ->
            [{_, Value}|_] = maps:to_list(Map),
            Value
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

%% @doc Convert a seconds-since-epoch timestamp to a Cassandra timestamp
%%
%% Cassandra timestamps are, internally, ms since epoch and can be passed
%% in in that format.
-spec seconds_to_timestamp(non_neg_integer()) -> non_neg_integer().
seconds_to_timestamp(S) ->
    S * 1000.

%% @doc Convert a Cassandra timestamp into seconds-since-epoch
%%
-spec timestamp_to_seconds(non_neg_integer()) -> non_neg_integer().
timestamp_to_seconds(S) ->
    S div 1000.

%% @doc Convert a Cassandra timestamp into {MegaSecs, Secs, MicroSecs}
%%
%% The expiry timestamps can also have a value of `never`. We store that as 0 in
%% Cassandra.
-spec timestamp_to_now(non_neg_integer()) ->
    {non_neg_integer(), non_neg_integer(), non_neg_integer()} | never.
timestamp_to_now(0) -> never;
timestamp_to_now(TimestampMS) ->
    MegaSecs = TimestampMS div 1000000000,
    Secs = (TimestampMS div 1000) - (MegaSecs * 1000000),
    MicroSecs = (TimestampMS rem 1000) * 1000,
    {MegaSecs, Secs, MicroSecs}.

%% @doc Convert a {MegaSecs, Secs, MicroSecs} time to a Cassandra timestamp
%%
-spec now_to_timestamp({non_neg_integer(), non_neg_integer(), non_neg_integer()}
                       | never) -> non_neg_integer().
now_to_timestamp(never) -> 0;
now_to_timestamp({MegaSecs, Secs, MicroSecs}) ->
    (MegaSecs * 1000000000) + (Secs * 1000) + (MicroSecs div 1000).

%% @doc Convert a now() style expiry time to a value for C*'s TTL
%%
%% Note that because C* will throw an error for non-positive values in TTL, we
%% clamp the return to no less than 1, allowing this function's result to be
%% safely passed straight into a TTL binding in a query. The 'never' atom is
%% left unchanged intentionally - it is up to the caller to avoid using the TTL
%% value at all in this case. Leaving the value as 'never' ensures it will cause
%% the query to fail if used.
%%
-spec expire_to_ttl(never | non_neg_integer()) -> never | pos_integer().
expire_to_ttl(never) -> never;
expire_to_ttl(Expire) ->
    Now = os:timestamp(),
    TTL = timer:now_diff(Now, Expire) div 1000000,
    lists:max([TTL, 1]).

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

make_query(Query, Values, Consistency) ->
    #cql_query{statement = Query,
               values = Values,
               reusable = true,
               consistency = Consistency}.

make_batch_query(QueryList, Consistency, Mode) ->
    #cql_query_batch{queries = batch_query_list(QueryList),
                     consistency = Consistency,
                     mode = Mode}.

batch_query_list(QueryList) ->
    lists:map(fun ({Query, Values}) ->
                      #cql_query{statement = Query, values = Values}
              end, QueryList).

drop_all_nulls(Rows) ->
    [drop_nulls(Row) || Row <- Rows].

drop_nulls(Row) ->
    maps:filter(fun (_, null) -> false;
                    (_, _) -> true end, Row).
