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
-include("wocky.hrl").

-type table_def() :: #table_def{}.
-type context()   :: none | shared | binary().
-type query()     :: iodata().
-type value()     :: parameter_val().
-type values()    :: maps:map().
-type row()       :: maps:map().
-type rows()      :: [row()].
-type error()     :: term().
-opaque result()  :: #cql_result{}.
-export_type([context/0, query/0, value/0, values/0,
              row/0, result/0, error/0]).


%% High Level API
-export([select/4, insert/3, insert_unique/3, truncate/2, drop/3,
         create_keyspace/3, create_table/2]).

%% Query API
-export([query/3, query/4, batch_query/4, multi_query/3, multi_query/4,
         rows/1, single_row/1, single_result/1, single_result/2, count/2]).

%% Utility API
-export([to_keyspace/1, keyspace_name/1, seconds_to_timestamp/1,
         timestamp_to_seconds/1, timestamp_to_now/1, now_to_timestamp/1,
         expire_to_ttl/1]).

-ifdef(TEST).
%% Query building functions exported for unit tests
-export([build_select_query/3, build_insert_query/2, build_truncate_query/1,
         build_drop_query/2, build_create_keyspace_query/3,
         build_create_table_query/1]).
-endif.


%%====================================================================
%% High Level API
%%====================================================================

%% @doc TBD
-spec select(context(), atom(), all | [atom()], [{atom(), term()}]) -> rows().
select(Context, Table, Columns, Conditions) ->
    Query = build_select_query(Table, Columns, Conditions),
    {ok, R} = query(Context, Query, Conditions, quorum),
    rows(R).

build_select_query(Table, Columns, Conditions) ->
    ["SELECT ", columns(Columns), " FROM ", atom_to_list(Table),
     conditions(Conditions)].

columns(all) -> "*";
columns([First|Rest]) ->
    lists:foldl(
      fun (Column, Str) -> [Str, ", ", atom_to_list(Column)] end,
      atom_to_list(First),
      Rest).

conditions([]) -> "";
conditions([{First, _}|Rest]) ->
    Start = [" WHERE ", atom_to_list(First), " = ?"],
    lists:foldl(
      fun ({Name, _}, Str) -> [Str, " AND ", atom_to_list(Name), " = ?"] end,
      Start,
      Rest).


%% @doc TBD
-spec insert(context(), atom(), maps:map()) -> ok.
insert(Context, Table, Values) ->
    Keys = maps:keys(Values),
    Query = build_insert_query(Table, Keys),
    {ok, void} = query(Context, Query, Values, quorum),
    ok.


%% @doc TBD
-spec insert_unique(context(), atom(), maps:map()) -> ok.
insert_unique(Context, Table, Values) ->
    Keys = maps:keys(Values),
    Query = [build_insert_query(Table, Keys), " IF NOT EXISTS"],
    {ok, R} = query(Context, Query, Values, quorum),
    single_result(R).

build_insert_query(Table, Keys) ->
    ["INSERT INTO ", atom_to_list(Table), " ", names(Keys),
     " VALUES ", placeholders(length(Keys))].

placeholders(1) -> "(?)";
placeholders(N) -> ["(", ["?, " || _X <- lists:seq(2, N)], "?)"].

names(Keys) ->
    names(lists:reverse(Keys), []).

names([Key], Acc) ->
    ["(", [atom_to_list(Key) | Acc], ")"];
names([Key | Keys], Acc) ->
    names(Keys, [[", ", atom_to_list(Key)] | Acc]).


%% @doc TBD
-spec truncate(context(), atom()) -> ok.
truncate(Context, Name) ->
    Query = build_truncate_query(Name),
    {ok, void} = query(Context, Query, all),
    ok.

build_truncate_query(Name) ->
    ["TRUNCATE TABLE ", atom_to_list(Name)].


%% @doc TBD
-spec drop(context(), atom(), atom()) -> ok.
drop(Context, Type, Name) ->
    Query = build_drop_query(Type, Name),
    {ok, void} = query(Context, Query, all),
    ok.

build_drop_query(Type, Name) ->
    ["DROP ", string:to_upper(atom_to_list(Type)), " ", atom_to_list(Name)].


%% @doc TBD
-spec create_keyspace(context(), simple | topology,
                      non_neg_integer() | [{binary(), non_neg_integer}]) -> ok.
create_keyspace(Context, Class, Factor) ->
    Query = build_create_keyspace_query(keyspace_name(Context), Class, Factor),
    {ok, void} = query(none, Query, all),
    ok.

build_create_keyspace_query(Name, Class, Factor) ->
    ["CREATE KEYSPACE IF NOT EXISTS ", atom_to_list(Name),
     " WITH REPLICATION = ", replication_strategy(Class, Factor)].

replication_strategy(simple, Factor) ->
    ["{'class': 'SimpleStrategy',"
     " 'replication_factor': ", integer_to_list(Factor), "}"];
replication_strategy(topology, DCs) ->
    ["{'class': 'NetworkTopologyStrategy'", dc_factors(DCs, []), "}"].

dc_factors([], Factors) ->
    Factors;
dc_factors([{DC, Factor} | Rest], Factors) ->
    FactorString = [", '", atom_to_list(DC), "': ", integer_to_list(Factor)],
    dc_factors(Rest, [Factors | FactorString]).


%% @doc TBD
-spec create_table(context(), table_def()) -> ok.
create_table(Context, TableDef) ->
    Query = build_create_table_query(TableDef),
    {ok, void} = query(Context, Query, all),
    ok.

build_create_table_query(TD) ->
    Name = atom_to_list(TD#table_def.name),
    ["CREATE TABLE IF NOT EXISTS ", Name, " (",
     column_strings(TD#table_def.columns),
     primary_key_string(TD#table_def.primary_key),
     ")", sorting_option_string(TD#table_def.order_by)].

column_strings(Cols) ->
    [[atom_to_list(CName), " ", atom_to_list(CType), ", "]
     || {CName, CType} <- Cols].

primary_key_string(PK) when is_atom(PK) ->
    primary_key_string([PK]);
primary_key_string(PK) ->
    primary_key_string(lists:reverse(PK), []).

primary_key_string([PK], Acc) ->
    ["PRIMARY KEY (", atom_to_list(PK), Acc, ")"];
primary_key_string([First | Rest], Acc) ->
    primary_key_string(Rest, [", ", atom_to_list(First)|Acc]).

sorting_option_string(undefined) -> "";
sorting_option_string(Field) when is_atom(Field) ->
    sorting_option_string([{Field, asc}]);
sorting_option_string([{Field, Dir}]) ->
    [" WITH CLUSTERING ORDER BY (", atom_to_list(Field), " ",
     string:to_upper(atom_to_list(Dir)), ")"].


%%====================================================================
%% Query API
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
% Cassandra throws an exception if you try to batch zero queries. Early-out
% here:
batch_query(_Context, [], _Mode, _Consistency) -> {ok, void};
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
-spec single_row(result()) -> row() | undefined.
single_row(Result) ->
    case cqerl:head(Result) of
        empty_dataset -> undefined;
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

%% @doc Extracts the value of the first column of the first row from a query
%% result. Returns `Default' if the result set is empty or if first value
%% is `null'.
%%
-spec single_result(result(), term()) -> term().
single_result(Result, Default) ->
    case single_result(Result) of
        undefined -> Default;
        null      -> Default;
        Value     -> Value
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


%%====================================================================
%% Utility API
%%====================================================================

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

%% @doc Return the keyspace name for the given context.
-spec keyspace_name(context) -> binary().
keyspace_name(Context) when is_atom(Context) ->
    keyspace_name(atom_to_list(Context));
keyspace_name(Context) ->
    iolist_to_binary([keyspace_prefix(), Context]).

keyspace_prefix() ->
    application:get_env(wocky, keyspace_prefix, "wocky_").

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
    TTL = timer:now_diff(Expire, Now) div 1000000,
    lists:max([TTL, 1]).


%%====================================================================
%% Internal functions
%%====================================================================

run_query(Context, Query) ->
    case get_client({}, Context) of
        {ok, Client} ->
            Return = cqerl:run_query(Client, Query),
            cqerl:close_client(Client),
            Return;
        {closed, Error} ->
            {error, Error}
    end.

get_client(Spec, none) ->
    cqerl:new_client(Spec);
get_client(Spec, Context) ->
    cqerl:new_client(Spec, [{keyspace, keyspace_name(Context)}]).

make_query(Query, Values, Consistency) ->
    log_query(Query, Values),
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
                      log_query(Query, Values),
                      #cql_query{statement = Query, values = Values}
              end, QueryList).

log_query(Query, Values) ->
    lager:info("Creating CQL query with statement ~s and values ~p",
               [Query, Values]).

drop_all_nulls(Rows) ->
    [drop_nulls(Row) || Row <- Rows].

drop_nulls(Row) ->
    maps:filter(fun (_, null) -> false;
                    (_, _) -> true end, Row).
