%%% @copyright 2015+ Hippware, Inc.
%%% @doc Cassandra dispatcher
%%%
%%% This initialises, and dispatches to, a configurable backend.
%%% The backend interfaces with the Cassandra driver.
%%%
%%% Whilst this is supposed to be a generic Cassandra interface, for practical
%%% purposes, it is still closely coupled to the backend. If we ever support
%%% more than one backend, then the interface needs to be made backend agnostic.
%%%
%%% The other reason for this module is to reduce verbosity.
%%% It's nicer to write cassandra:func() rather than cassandra_seestar:func().
%%%
%%% Generally, use prepared queries unless there is a good reason not to.
%%%
%%% The query functions are spec-ced to return `{ok, result()}' but usually
%%% return `{ok, rows_result()}'. `result()' is a generalisation/superset of
%%% `rows_result()'.

-module(wocky_db).


%% Borrow definitions from seestar. If we change backend, then this may change too.
-type value() :: seestar_cqltypes:value().
-type consistency() :: any | one | two | three | quorum | all | local_quorum | each_quorum.
-type result() :: seestar_result:result().
-type error() :: seestar_error:error().
-export_type([value/0, consistency/0, result/0, error/0]).
-type rows_result() :: seestar_result:rows_result().

%% API
-export([maybe_configure/0,
         configure/0,
         configure/2,
         clear/0]).

-export([query/3, query/4, query/5,
         query_async/3, query_async/4, query_async/5,
         batch_query/4,
         rows/1, single_result/1, boolean_result/1,
         to_keyspace/1]).


%%====================================================================
%% Backend configuration
%%====================================================================

%% @doc Calls configure/0 if a configuration has been provided in the
%% application environment. If no configuration is available, do nothing.
-spec maybe_configure() -> ok | {error, any()}.
maybe_configure() ->
    case config_env() of
        {ok, Host, Config} ->
            configure(Host, Config);

        {error, no_config} ->
            %% Silently continue
            %% TODO: how do we handle lager in eunit tests?
            %% lager:warning("No configuration found. Starting unconfigured!"),
            ok
    end.

%% @doc Configure the Cassandra backend using settings in the application
%% environment. The settings are backend-dependent and should be in a tuple
%% that is named the same as the backed; e.g, the configuration tuple for
%% wocky_db will be in a tuple that looks like {wocky_db, [...]}.
%%
%% This function can be called multiple times and the configuration will be
%% merged with the curent configuration with more recent entries overwriting
%% older ones.
-spec configure() -> ok | {error, any()}.
configure() ->
    case config_env() of
        {ok, Host, Config} -> configure(Host, Config);
        {error, no_config} = E -> E
    end.

%% @doc Configure the Cassandra backend using settings passed as arguments. The
%% settings should be in the same format as if they were provided in the application
%% environment.
%%
%% This function can be called multiple times and the configuration will be
%% merged with the curent configuration with more recent entries overwriting
%% older ones.
-spec configure(Host :: string(), Config :: [proplists:property()]) -> ok | {error, any()}.
configure(Host, Config) ->
    wocky_db_seestar:configure(Host, Config).

%% @doc Clear the Cassandra backend configuration. After this call, the backend
%% will be in a pristine state and it will be safe to call configure/0 or
%% configure/2 again.
-spec clear() -> ok.
clear() ->
    wocky_db_seestar:clear().


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
    wocky_db_seestar:query(Host, Query, Values, Consistency, PageSize).

%% @doc A wrapper around {@link query_async/5}
%% @spec query_async(Host, Query, Consistency) -> ok
query_async(Host, Query, Consistency) ->
    query_async(Host, Query, [], Consistency, undefined).

%% @doc A wrapper around {@link query_async/5}
%% @spec query_async(Host, Query, Values, Consistency) -> ok
query_async(Host, Query, Values, Consistency) when is_list(Values) ->
    query_async(Host, Query, Values, Consistency, undefined);

query_async(Host, Query, Consistency, PageSize) when is_atom(PageSize) ->
    query_async(Host, Query, [], Consistency, PageSize).

%% @doc Execute an asynchronous prepared query (in the context of a virtual host).
%%
%% `Query' is a query string where '?' characters are substituted with
%% parameters from the `Values' list.
%% `Host' is the virtual host to execute the query for.
%%
%% ToDo: The return value and how to signal the result to the caller hasn't
%% been determined. Needs review and may change in the future.
%%
-spec query_async(Host, Query, Values, Consistency, PageSize) -> ok when
             Host :: binary(),
             Query :: binary() | string(),
             Values :: [value()],
             Consistency :: consistency(),
             PageSize :: non_neg_integer() | undefined.
query_async(Host, Query, Values, Consistency, PageSize) ->
    wocky_db_seestar:query_async(Host, Query, Values, Consistency, PageSize).

%% @doc Executes a batch of queries as prepared queries (in the context of a virtual host).
%%
%% `Queries' is a list of `{Query, Values}' tuples where
%% `Query' is a query string where '?' characters are substituted with
%% parameters from the list `Values'.
%%
-spec batch_query(Host, Queries, Type, Consistency) -> {ok, Result :: seestar_result:result()} | {error, Error :: seestar_error:error()} when
             Host :: binary(),
             Queries :: [{binary() | string(), [value()]}],
             Type :: logged | unlogged | counter,
             Consistency :: consistency().
batch_query(Host, Queries, Type, Consistency) ->
    wocky_db_seestar:batch_query(Host, Queries, Type, Consistency).

%% @doc Extracts rows from a query result
%%
%% Returns a list of rows. Each row is a list of values.
%%
-spec rows(Result :: rows_result()) -> [[value()]].
rows(Result) ->
    wocky_db_seestar:rows(Result).

%% @doc Extracts the value of the first column of the first row from a query result
%%
-spec single_result(Result :: rows_result()) -> value() | undefined.
single_result(Result) ->
    case rows(Result) of
        [] -> undefined;
        [[Value | _] | _] -> Value
    end.

%% @doc Extracts the boolean value from the result of a 'IF NOT EXISTS' query
%%
-spec boolean_result(Result :: rows_result()) -> boolean().
boolean_result(Result) ->
    %% Note: Result is <<1>> for success, <<0>> if error.
    %% There is no documentation on the return type so it's possible,
    %%   in the future, this may not be a binary.
    single_result(Result) /= <<0>>.

%% @doc Modify a string so it is a valid keyspace
%%
%% All invalid characters are replaced with underscore and then
%% truncated to 48 characters. Returns the modified string.
%%
-spec to_keyspace(String :: binary()) -> binary().
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

config_env() ->
    HostRes = application:get_env(wocky, host),
    ConfigRes = application:get_env(wocky, wocky_db),
    case {HostRes, ConfigRes} of
        {{ok, Host}, {ok, Config}} -> {ok, Host, Config};
        _Else -> {error, no_config}
    end.
