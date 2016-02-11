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

-type context()    :: none | shared | binary().
-type table()      :: atom().
-type columns()    :: all | [atom()].
-type conditions() :: map().
-type ks_class()   :: simple | topology.
-type ks_factor()  :: non_neg_integer() | [{binary(), non_neg_integer}].
-type table_def()  :: #table_def{}.
-export_type([context/0, table/0, columns/0, conditions/0,
              ks_class/0, ks_factor/0, table_def/0]).

-type query()     :: iodata().
-type value()     :: parameter_val().
-type values()    :: map().
-type row()       :: map().
-type rows()      :: [row()].
-type error()     :: term().
-type result()    :: #cql_result{}.
-export_type([query/0, value/0, values/0, row/0, result/0, error/0]).


%% High Level API
-export([select_one/4, select_row/4, select/4, insert/3, insert_new/3, update/4,
         delete/4, count/3, truncate/2, drop/3, create_keyspace/3,
         create_table/2, create_index/3, create_view/6]).

%% Query API
-export([query/4, batch_query/4, multi_query/3, multi_query/4, rows/1,
         single_result/1]).

%% Utility API
-export([seconds_to_timestamp/1, timestamp_to_seconds/1, timestamp_to_now/1,
         now_to_timestamp/1, expire_to_ttl/1, drop_all_nulls/1, drop_nulls/1]).

-ifdef(TEST).
%% Query building functions exported for unit tests
-export([to_keyspace/1]).
-export([build_select_query/3, build_insert_query/3, build_delete_query/3,
         build_update_query/3, build_truncate_query/1, build_drop_query/2,
         build_create_keyspace_query/3, build_create_table_query/1,
         build_create_index_query/2, build_create_view_query/5]).
-endif.


%%====================================================================
%% High Level API
%%====================================================================

%% @doc Retrieves a single value from a table based on the parameters.
%% Returns the first value of the first row.
-spec select_one(context(), table(), atom(), conditions())
                -> term() | not_found.
select_one(Context, Table, Column, Conditions) ->
    {ok, R} = run_select_query(Context, Table, [Column], Conditions),
    single_result(R).

%% @doc Retrieves a single row from a table based on the parameters.
%% Returns the first row of the result set.
-spec select_row(context(), table(), columns(), conditions())
                -> row() | not_found.
select_row(Context, Table, Columns, Conditions) ->
    {ok, R} = run_select_query(Context, Table, Columns, Conditions),
    single_row(R).

%% @doc Retrieves data from a table based on the parameters and
%% returns all rows of the result set.
-spec select(context(), table(), columns(), conditions()) -> rows().
select(Context, Table, Columns, Conditions) ->
    {ok, R} = run_select_query(Context, Table, Columns, Conditions),
    rows(R).

run_select_query(Context, Table, Columns, Conditions) ->
    Query = build_select_query(Table, Columns, keys(Conditions)),
    query(Context, Query, Conditions, quorum).

keys(Map) -> maps:keys(Map).

build_select_query(Table, Columns, Keys) ->
    ["SELECT", columns(Columns, " * "), "FROM ", atom_to_list(Table),
     conditions(Keys)].

columns(all, Default) -> Default;
columns([], Default) -> Default;
columns([First|Rest], _) ->
    [lists:foldl(
       fun (Column, Str) -> [Str, ", ", atom_to_list(Column)] end,
       [" ", atom_to_list(First)],
       Rest), " "].

conditions([]) -> "";
conditions([First|Rest]) ->
    lists:foldl(
      fun (Name, Str) -> [Str, " AND ", atom_to_list(Name), " = ?"] end,
      [" WHERE ", atom_to_list(First), " = ?"],
      Rest).


%% @doc Inserts the provided row into the table.
-spec insert(context(), table(), values()) -> ok.
insert(Context, Table, Values) ->
    {ok, void} = run_insert_query(Context, Table, Values, false),
    ok.

%% @doc Inserts the provided row into the table using a Lightweight Transaction.
-spec insert_new(context(), table(), values()) -> boolean().
insert_new(Context, Table, Values) ->
    {ok, R} = run_insert_query(Context, Table, Values, true),
    single_result(R).

run_insert_query(Context, Table, Values, UseLWT) ->
    Query = build_insert_query(Table, keys(Values), UseLWT),
    query(Context, Query, Values, quorum).

build_insert_query(Table, AllKeys, UseLWT) ->
    {TTL, Keys} = lists:partition(fun (K) -> K =:= '[ttl]' end, AllKeys),
    ["INSERT INTO ", atom_to_list(Table), " ", names(Keys),
     " VALUES ", placeholders(length(Keys)), use_lwt(UseLWT),
     ttl_option(TTL)].

placeholders(1) -> "(?)";
placeholders(N) -> ["(", lists:duplicate(N - 1, "?, "), "?)"].

names(Keys) ->
    names(lists:reverse(Keys), []).

names([Key], Acc) ->
    ["(", [atom_to_list(Key) | Acc], ")"];
names([Key | Keys], Acc) ->
    names(Keys, [[", ", atom_to_list(Key)] | Acc]).

use_lwt(false) -> "";
use_lwt(true) -> " IF NOT EXISTS".

ttl_option([]) -> "";
ttl_option(['[ttl]']) -> " USING TTL ?".


%% @doc Updates rows in the table based on the parameters.
-spec update(context(), table(), conditions(), conditions()) -> ok.
update(Context, Table, Updates, Conditions) ->
    Values = maps:merge(Updates, Conditions),
    Query = build_update_query(Table, keys(Updates), keys(Conditions)),
    {ok, _} = query(Context, Query, Values, quorum),
    ok.

build_update_query(Table, Columns, Keys) ->
    ["UPDATE ", atom_to_list(Table), " SET ", update_columns(Columns),
     conditions(Keys)].

update_columns([First|Rest]) ->
    lists:foldl(
      fun (Name, Str) -> [Str, ", ", atom_to_list(Name), " = ?"] end,
      [atom_to_list(First), " = ?"],
      Rest).


%% @doc Deletes rows from a table.
-spec delete(context(), table(), columns(), conditions()) -> ok.
delete(Context, Table, Columns, Conditions) ->
    Query = build_delete_query(Table, Columns, keys(Conditions)),
    {ok, void} = query(Context, Query, Conditions, quorum),
    ok.

build_delete_query(Table, Columns, Keys) ->
    ["DELETE", columns(Columns, " "), "FROM ", atom_to_list(Table),
     conditions(Keys)].

%% @doc Counts the rows in a table matching the conditions
-spec count(context(), table(), conditions()) -> non_neg_integer().
count(Context, Table, Conditions) ->
    Query = build_count_query(Table, keys(Conditions)),
    {ok, Result} = query(Context, Query, Conditions, quorum),
    single_result(Result).

build_count_query(Table, Keys) ->
    ["SELECT COUNT(*) FROM ", atom_to_list(Table), conditions(Keys)].


%% @doc Deletes all data in a table.
-spec truncate(context(), table()) -> ok.
truncate(Context, Name) ->
    Query = build_truncate_query(Name),
    {ok, void} = query(Context, Query, #{}, all),
    ok.

build_truncate_query(Name) ->
    ["TRUNCATE TABLE ", atom_to_list(Name)].


%% @doc Drops the specified database artifact if it exists.
-spec drop(context(), atom(), atom()) -> ok.
drop(Context, Type, Name) ->
    Query = build_drop_query(Type, Name),
    {ok, _} = query(Context, Query, #{}, all),
    ok.

build_drop_query(Type, Name) ->
    ["DROP ", string:to_upper(atom_to_list(Type)), " IF EXISTS ",
     atom_to_list(Name)].


%% @doc Creates a keyspace if it does not already exist.
-spec create_keyspace(context(), ks_class(), ks_factor()) -> ok.
create_keyspace(Context, Class, Factor) ->
    Query = build_create_keyspace_query(keyspace_name(Context), Class, Factor),
    {ok, _} = query(none, Query, #{}, all),
    ok.

build_create_keyspace_query(Name, Class, Factor) ->
    ["CREATE KEYSPACE IF NOT EXISTS ", binary_to_list(Name),
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


%% @doc Creates a table if it does not already exist.
-spec create_table(context(), table_def()) -> ok.
create_table(Context, TableDef) ->
    Query = build_create_table_query(TableDef),
    {ok, _} = query(Context, Query, #{}, all),
    ok.

build_create_table_query(TD) ->
    ["CREATE TABLE IF NOT EXISTS ", atom_to_list(TD#table_def.name), " (",
     column_strings(TD#table_def.columns),
     primary_key_string(TD#table_def.primary_key),
     ")", sorting_option_string(TD#table_def.order_by)].

column_strings(Cols) ->
    [[atom_to_list(CName), " ", column_type(CType), ", "]
     || {CName, CType} <- Cols].

column_type({map, Type1, Type2}) ->
    ["map<", atom_to_list(Type1), ",", atom_to_list(Type2), ">"];
column_type({Coll, Type}) when Coll =:= set; Coll =:= list ->
    [atom_to_list(Coll), "<", atom_to_list(Type), ">"];
column_type(Type) when is_atom(Type) ->
    atom_to_list(Type).

primary_key_string(PK) when is_atom(PK) ->
    primary_key_string([PK]);
primary_key_string(PK) ->
    primary_key_string(lists:reverse(PK), []).

primary_key_string([PK], Acc) ->
    ["PRIMARY KEY (", primary_key_element(PK), Acc, ")"];
primary_key_string([First | Rest], Acc) ->
    primary_key_string(Rest, [", ", primary_key_element(First)|Acc]).

primary_key_element(Columns) when is_list(Columns) ->
    ["(", columns(Columns, ""), ")"];
primary_key_element(Col) when is_atom(Col) ->
    atom_to_list(Col).

sorting_option_string([]) -> "";
sorting_option_string(Field) when is_atom(Field) ->
    sorting_option_string([{Field, asc}]);
sorting_option_string([{Field, Dir}]) ->
    [" WITH CLUSTERING ORDER BY (", atom_to_list(Field), " ",
     string:to_upper(atom_to_list(Dir)), ")"].


%% @doc Creates an index if it does not already exist.
-spec create_index(context(), table(), [atom()]) -> ok.
create_index(Context, Table, Keys) ->
    Query = build_create_index_query(Table, Keys),
    {ok, _} = query(Context, Query, #{}, all),
    ok.

build_create_index_query(Table, Keys) ->
    ["CREATE INDEX IF NOT EXISTS ON ", atom_to_list(Table), index_keys(Keys)].

index_keys(Keys) ->
    index_keys(lists:reverse(Keys), []).

index_keys([K], Acc) ->
    [" (", atom_to_list(K), Acc, ")"];
index_keys([First | Rest], Acc) ->
    index_keys(Rest, [", ", atom_to_list(First)|Acc]).


%% @doc Creates a materialized view if it does not already exist.
-spec create_view(context(), atom(), table(), [atom() | all], [atom()],
                  [{atom, asc | desc}]) -> ok.
create_view(Context, Name, Table, Columns, Keys, OrderBy) ->
    Query = build_create_view_query(Name, Table, Columns, Keys, OrderBy),
    {ok, _} = query(Context, Query, #{}, all),
    ok.

build_create_view_query(Name, Table, Columns, Keys, OrderBy) ->
    ["CREATE MATERIALIZED VIEW IF NOT EXISTS ", atom_to_list(Name), " AS ",
     "SELECT", columns(Columns, " * "), "FROM ", atom_to_list(Table),
     view_conditions(Keys), " ", primary_key_string(Keys),
     sorting_option_string(OrderBy)].

view_conditions([First|Rest]) ->
    lists:foldl(
      fun (Name, Str) -> [Str, " AND ", atom_to_list(Name), " IS NOT NULL"] end,
      [" WHERE ", atom_to_list(First), " IS NOT NULL"],
      Rest).


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
    cqerl:all_rows(Result).


%%====================================================================
%% Utility API
%%====================================================================

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
-spec expire_to_ttl(never | non_neg_integer()) -> infinity | pos_integer().
expire_to_ttl(never) -> infinity;
expire_to_ttl(Expire) ->
    Now = os:timestamp(),
    TTL = timer:now_diff(Expire, Now) div 1000000,
    lists:max([TTL, 1]).

%% @doc TBD
-spec drop_all_nulls(rows()) -> rows().
drop_all_nulls(Rows) ->
    [drop_nulls(Row) || Row <- Rows].

%% @doc TBD
-spec drop_nulls(row()) -> row().
drop_nulls(Row) ->
    maps:filter(fun (_, null) -> false;
                    (_, _) -> true end, Row).


%%====================================================================
%% Internal functions
%%====================================================================

default_db_config() ->
    application:get_env(wocky, cqerl_node, {}).

get_db_config() ->
    %% Try pulling the config from ejabberd
    try
        case ejabberd_config:get_global_option(cqerl_node) of
            undefined -> default_db_config();
            Value -> Value
        end
    catch
        _:_ ->
            default_db_config()
    end.

run_query(Context, Query) ->
    Spec = get_db_config(),
    case get_client(Spec, Context) of
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

%% Return the keyspace name for the given context.
-spec keyspace_name(context()) -> binary().
keyspace_name(Context) when is_atom(Context) ->
    keyspace_name(atom_to_binary(Context, utf8));
keyspace_name(Context) ->
    to_keyspace([keyspace_prefix(), Context]).

keyspace_prefix() ->
    {ok, Prefix} = application:get_env(wocky, keyspace_prefix),
    Prefix.

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
    ok = lager:info("Creating CQL query with statement '~s' and values ~p",
                    [Query, Values]).

%% Extracts the value of the first column of the first row from a query result
-spec single_result(result()) -> term() | not_found.
single_result(Result) ->
    case cqerl:head(Result) of
        empty_dataset ->
            not_found;
        Map ->
            [{_, Value}|_] = maps:to_list(Map),
            Value
    end.

%% Extracts the first row from a query result
%% The row is a property list of column name, value pairs.
-spec single_row(result()) -> row() | not_found.
single_row(Result) ->
    case cqerl:head(Result) of
        empty_dataset -> not_found;
        R -> R
    end.
