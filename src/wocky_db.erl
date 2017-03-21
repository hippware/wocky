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
-include("wocky.hrl").

-define(schemata, 'Elixir.Schemata').
-define(query, 'Elixir.Schemata.Query').
-define(result, 'Elixir.Schemata.Result').
-define(schema, 'Elixir.Schemata.Schema').
-define(timex, 'Elixir.Timex').

-type server()     :: binary().
-type context()    :: none | shared | server().
-type table()      :: atom().
-type column()     :: atom().
-type explicit_columns() :: [column()].
-type columns()    :: all | explicit_columns().
-type conditions() :: map().
-type ks_class()   :: simple | topology.
-type ks_factor()  :: non_neg_integer() | [{binary(), non_neg_integer}].
-export_type([server/0, context/0, table/0, explicit_columns/0, columns/0,
              conditions/0, ks_class/0, ks_factor/0]).

-type statement() :: ?query:statement().
-type value()     :: ?query:parameter_val().
-type values()    :: map().
-type row()       :: map().
-type rows()      :: [row()].
-type error()     :: term().
-type ttl()       :: pos_integer() | infinity.
-type id()        :: binary().
-type result()    :: ?result:t().
-export_type([statement/0, value/0, values/0, row/0, result/0, error/0,
              ttl/0, id/0]).

-type consistency_level() :: ?query:consistency_level().

%% High Level API
-export([shared_keyspace/0, local_keyspace/0,
         bootstrap/0, bootstrap/1, create_schema/0, keyspace_tables/1,
         clear_tables/2, clear_user_tables/1,
         select_one/4, select_row/4, select_column/4,
         select/4, insert/3, insert_new/3,
         update/4, delete/4, count/3, truncate/2]).

%% Query API
-export([query/4, batch_query/3, multi_query/3, multi_query/4, rows/1,
         all_rows/1, single_result/1, fetch_more/1]).

%% Utility API
-export([timestamp_to_string/1, seconds_to_timestamp/1, timestamp_to_seconds/1,
         timestamp_to_now/1, now_to_timestamp/1,
         expire_to_ttl/1, drop_nulls/1, keyspace_name/1]).

-ifdef(TEST).
-export([to_keyspace/1]).
-endif.


%%====================================================================
%% High Level API
%%====================================================================

-spec shared_keyspace() -> binary().
shared_keyspace() ->
    keyspace_name(<<"shared">>).

-spec local_keyspace() -> binary().
local_keyspace() ->
    keyspace_name(wocky_app:server()).

%% @doc Create the schema for both the shared and local keyspaces then
%% load seed data into the tables.
-spec bootstrap() -> ok.
bootstrap() ->
    bootstrap(shared),
    bootstrap(wocky_app:server()).

%% @doc Create the schema for both the specified keyspace then
%% load seed data into the tables.
-spec bootstrap(context()) -> ok.
bootstrap(Context) ->
    create_schema_for(Context),
    wocky_db_seed:seed_keyspace(Context, wocky_app:server()).


%% @doc Create the schema for both the shared and local keyspaces.
-spec create_schema() -> ok.
create_schema() ->
    create_schema(wocky_app:server()).

%% @private
create_schema(Context) ->
    create_schema_for(shared),
    create_schema_for(Context).

%% @private
create_schema_for(Context) ->
    ok = ?schema:create_keyspace(keyspace_name(Context)),
    ok.


%% @doc List the tables defined for the specified keyspace.
-spec keyspace_tables(context()) -> [table()].
keyspace_tables(Context) ->
    ?schema:list_tables(keyspace_name(Context)).


%% @private
foreach_table(Context, Fun, Tables) ->
    lists:foreach(fun (Table) -> ok = Fun(Context, Table) end, Tables).


%% @doc Truncate the specified tables.
-spec clear_tables(context(), [table()]) -> ok.
clear_tables(Context, Tables) ->
    foreach_table(Context, fun truncate/2, Tables).

%% @doc Truncate the tables related to user data.
-spec clear_user_tables(context()) -> ok.
clear_user_tables(Context) ->
    clear_tables(shared, [user, handle_to_user, phone_number_to_user, roster]),
    clear_tables(Context, [auth_token, privacy, privacy_item, location,
                           conversation]).


%% @doc Retrieves a single value from a table based on the parameters.
%% Returns the first value of the first row.
-spec select_one(context(), table(), atom(), conditions())
                -> term() | not_found.
select_one(Context, Table, Column, Conditions) ->
    R = run_select_query(Context, Table, [Column], Conditions, 1),
    single_result(R).

%% @doc Retrieves a single row from a table based on the parameters.
%% Returns the first row of the result set.
-spec select_row(context(), table(), columns(), conditions())
                -> row() | not_found.
select_row(Context, Table, Columns, Conditions) ->
    R = run_select_query(Context, Table, Columns, Conditions, 1),
    single_row(R).

%% @doc Retrives all values from a single column of the table for rows matching
%% the conditions.
-spec select_column(context(), table(), column(), conditions())
                -> [term()].
select_column(Context, Table, Column, Conditions) ->
    R = run_select_query(Context, Table, [Column], Conditions),
    [V || #{Column := V} <- all_rows(R)].

%% @doc Retrieves data from a table based on the parameters and
%% returns all rows of the result set.
-spec select(context(), table(), columns(), conditions()) -> rows().
select(Context, Table, Columns, Conditions) ->
    R = run_select_query(Context, Table, Columns, Conditions),
    all_rows(R).

run_select_query(Context, Table, Columns, Conditions) ->
    run_select_query(Context, Table, Columns, Conditions, nil).

run_select_query(Context, Table, Columns, Conditions, Limit) ->
    Opts = [{from, Table}, {in, keyspace_name(Context)},
            {values, Columns},
            {where, Conditions},
            {limit, Limit},
            {with, quorum}],

    Query = 'Elixir.Schemata.Query.Select':from_opts(Opts),
    ?query:'run!'(Query).


%% @doc Inserts the provided row into the table.
-spec insert(context(), table(), values()) -> ok.
insert(Context, Table, Values) ->
    void = run_insert_query(Context, Table, Values, false),
    ok.

%% @doc Inserts the provided row into the table using a Lightweight Transaction.
-spec insert_new(context(), table(), values()) -> boolean().
insert_new(Context, Table, Values) ->
    R = run_insert_query(Context, Table, Values, true),
    single_result(R).

run_insert_query(Context, Table, AllValues, UseLWT) ->
    {TTL, Values} = take_ttl(AllValues),
    Opts = [{into, Table}, {in, keyspace_name(Context)},
            {values, Values}, {ttl, TTL},
            {unique, UseLWT},
            {with, quorum}],

    Query = 'Elixir.Schemata.Query.Insert':from_opts(Opts),
    ?query:'run!'(Query).

take_ttl(Values = #{'[ttl]' := infinity}) ->
    {nil, maps:remove('[ttl]', Values)};
take_ttl(Values = #{'[ttl]' := TTL}) ->
    {TTL, maps:remove('[ttl]', Values)};
take_ttl(Values) ->
    {nil, Values}.


%% @doc Updates rows in the table based on the parameters.
-spec update(context(), table(), conditions(), conditions()) -> ok.
update(Context, Table, Updates, Conditions) ->
    Keyspace = keyspace_name(Context),
    ?schemata:update(Table, [{in, Keyspace},
                             {set, Updates},
                             {where, Conditions},
                             {with, quorum}]),
    ok.


%% @doc Deletes rows from a table.
-spec delete(context(), table(), columns(), conditions()) -> ok.
delete(Context, Table, Columns, Conditions) ->
    Keyspace = keyspace_name(Context),
    ?schemata:delete([{from, Table}, {in, Keyspace},
                      {values, Columns},
                      {where, Conditions},
                      {with, quorum}]),
    ok.


%% @doc Counts the rows in a table matching the conditions
-spec count(context(), table(), conditions()) -> non_neg_integer().
count(Context, Table, Conditions) ->
    Keyspace = keyspace_name(Context),
    ?schemata:count(Table, [{in, Keyspace}, {where, Conditions}]).


%% @doc Deletes all data in a table.
-spec truncate(context(), table()) -> ok.
truncate(Context, Name) ->
    Keyspace = keyspace_name(Context),
    ?schemata:truncate([{table, Name}, {in, Keyspace}]),
    ok.


%%====================================================================
%% Query API
%%====================================================================

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
-spec query(context(), statement(), values(), consistency_level())
           -> {ok, void} | {ok, result()} | {error, error()}.
query(Context, Query, Values, Consistency) ->
    ?query:run(keyspace_name(Context), Query, Values, Consistency).

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
%% On successful completion, the function returns `{ok, void}'.
%%
-spec batch_query(context(), [{statement(), values()}], consistency_level())
                 -> {ok, void} | {error, error()}.
batch_query(Context, QueryList, Consistency) ->
    ?query:run_batch(keyspace_name(Context), QueryList, Consistency).

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
-spec multi_query(context(), statement(), [values()], consistency_level()) ->
    ok.
multi_query(Context, Query, ValuesList, Consistency) ->
    ?query:run_multi(keyspace_name(Context), Query, ValuesList, Consistency),
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
-spec multi_query(context(), [{statement(), values()}], consistency_level()) ->
    ok.
multi_query(Context, QueryVals, Consistency) ->
    ?query:run_multi(keyspace_name(Context), QueryVals, Consistency),
    ok.

%% @doc Extracts rows from a query result
%%
%% Returns a list of rows. Each row is a map list of column name, value
%% pairs.
%%
%% NOTE: the number of rows returned may be limited by cqerl's internal
%% per-retrieval limit of 100. To get all rows you can use all_rows/1.
%%
-spec rows(result()) -> rows().
rows(Result) ->
    ?result:all_rows(Result).

%% @doc Fetch all rows from a mult-row query
%%
%% Returns the full set of rows from a query, fetching multiple pages if
%% required.
%%
%% USE WITH CARE: This function has no upper bound on the number of results
%% it attempts to fetch.
%%
-spec all_rows(result()) -> rows().
all_rows(Result) ->
    all_rows({ok, Result}, []).

all_rows(no_more_results, Acc) -> Acc;
all_rows({ok, ResultData}, Acc) ->
    all_rows(fetch_more(ResultData),
             Acc ++ rows(ResultData)).

%% Extracts the value of the first column of the first row from a query result
-spec single_result(result()) -> ?result:value() | not_found.
single_result(Result) ->
    ?result:single_value(Result).

%% Extracts the first row from a query result
%% The row is a property list of column name, value pairs.
-spec single_row(result()) -> row() | not_found.
single_row(Result) ->
    case ?result:head(Result) of
        empty_dataset -> not_found;
        R -> R
    end.

%% @doc Fetch more results from a multi-page query
%%
%% Returns the next result object, or `no_more_results' if the end of the
%% result set has been reached.
%%
-spec fetch_more(result()) -> {ok, result()} | no_more_results.
fetch_more(Result) ->
    ?result:fetch_more(Result).

%%====================================================================
%% Utility API
%%====================================================================

%% @doc Convert a Cassandra timestamp to an ISO-8601 string
-spec timestamp_to_string(non_neg_integer()) -> binary().
timestamp_to_string(TS) ->
    Time = ?timex:from_unix(timestamp_to_seconds(TS)),
    {ok, Binary} = ?timex:format(Time, <<"{ISO:Extended}">>),
    Binary.

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
%% The expiry timestamps can also have a value of `never'. We store that as 0 in
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
%% safely passed straight into a TTL binding in a query.
%%
-spec expire_to_ttl(never | non_neg_integer()) -> ttl().
expire_to_ttl(never) -> infinity;
expire_to_ttl(Expire) ->
    Now = os:timestamp(),
    TTL = timer:now_diff(Expire, Now) div 1000000,
    lists:max([TTL, 1]).

%% @doc TBD
-spec drop_nulls(row()) -> row().
drop_nulls(not_found) -> not_found;
drop_nulls(Row) ->
    maps:filter(fun (_, null) -> false;
                    (_, _) -> true end, Row).

%% Return the keyspace name for the given context.
-spec keyspace_name(context()) -> binary() | undefined.
keyspace_name(none) ->
    undefined;
keyspace_name(Context) when is_atom(Context) ->
    keyspace_name(atom_to_binary(Context, utf8));
keyspace_name(Context) ->
    to_keyspace([keyspace_prefix(), Context]).

keyspace_prefix() ->
    wocky_app:get_config(keyspace_prefix, <<"wocky_">>).

%% Modify a string so it is a valid keyspace
%% All invalid characters are replaced with underscore and then
%% truncated to 48 characters. Returns the modified string.
-spec to_keyspace(iodata()) -> binary().
to_keyspace(String) ->
    Space = iolist_to_binary(re:replace(String, "[^0-9a-z]", "_", [global])),
    case byte_size(Space) of
        Size when Size > 48 ->
            {Head, _} = split_binary(Space, 48),
            Head;
        _ ->
            Space
    end.
