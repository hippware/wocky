%%% @copyright 2015+ Hippware, Inc.
%%% @doc Cassandra dispatcher 
%%%
%%% This initialises, and dispatches to, a configurable backend. 
%%% The backend interfaces with the Cassandra driver.
%%% 
%%% This is implemented as an ejabberd module. 
%%% To use it, add this to the modules list in ejabberd.cfg (preferably first):
%%%
%%% `{cassandra, [{backend, <backend>}]},'
%%%
%%% Other values can be added to the module options list. They are
%%% backend-specific and will be passed as-is to the backend module.
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
%%% The query functions are spec-ced to return `{ok, result()}' but usually return `{ok, rows_result()}'. `result()' is a generalisation/superset of `rows_result()'.

-module(cassandra).

-define(BACKEND, (cassandra_backend:backend())).

% Borrow definitions from seestar. If we change backend, then this may change too.
-type value() :: seestar_cqltypes:value().
% seestar:consistency()
-type consistency() :: any | one | two | three | quorum | all | local_quorum | each_quorum.
-type result() :: seestar_result:result().
-type error() :: seestar_error:error().
-export_type([value/0, consistency/0, result/0, error/0]).
-type rows_result() :: seestar_result:rows_result().

%% API
-export([aquery/3, aquery/4, aquery/5, 
         pquery/3, pquery/4, pquery/5,
         pquery_async/3, pquery_async/4, pquery_async/5,
         batch_pquery/4,
         rows/1, single_result/1, boolean_result/1,
         uuid1/1, uuid4/1, timeuuid/1, to_keyspace/1]).

%% gen_mod
-behaviour(gen_mod).
-export([start/2, stop/1]).

%% gen_mod callbacks
%% @doc Start the module
start(Host, Opts) ->
    start_backend_module(Opts),
    ?BACKEND:start(Host, Opts).

%% @doc Stop the module
stop(Host) ->
    ?BACKEND:stop(Host).

%%====================================================================
%% Dynamic modules
%%====================================================================

start_backend_module(Opts) ->
    Backend = gen_mod:get_opt(backend, Opts, seestar),
    {Mod, Code} = dynamic_compile:from_string(cassandra_backend(Backend)),
    code:load_binary(Mod, "cassandra_backend.erl", Code).

-spec cassandra_backend(atom()) -> string().
cassandra_backend(Backend) when is_atom(Backend) ->
    lists:flatten(
        ["-module(cassandra_backend).
        -export([backend/0]).

        -spec backend() -> atom().
        backend() ->
            cassandra_",
            atom_to_list(Backend),
            ".\n"]).

%%====================================================================
%% API
%%====================================================================

%% @doc A wrapper around {@link aquery/5}
%% @spec aquery(Host, Query, Consistency) -> {ok, Result :: result()} | {error, Error :: error()}
aquery(Host, Query, Consistency) ->
    aquery(Host, Query, [], Consistency, undefined).

%% @doc A wrapper around {@link aquery/5}
%% @spec aquery(Host, Query, Values, Consistency) -> {ok, Result :: result()} | {error, Error :: error()}
aquery(Host, Query, Values, Consistency) when is_list(Values) ->
    aquery(Host, Query, Values, Consistency, undefined);

aquery(Host, Query, Consistency, PageSize) when is_atom(PageSize) ->
    aquery(Host, Query, [], Consistency, PageSize).

%% @doc Execute an ad-hoc query (in the context of a virtual host).
%% 
%% `Query' is a query string where '?' characters are substituted with 
%% parameters from the `Values' list. 
%% `Host' is the virtual host to execute the query for.
%% 
%% Using this function is discouraged as guessing the type for `Values' 
%% is not always correct, and prepared queries are usually more efficient.
%% 
-spec aquery(Host, Query, Values, Consistency, PageSize) -> {ok, Result :: result()} | {error, Error :: error()} when
             Host :: binary(),
             Query :: binary() | string(), % seestar_session:'query'()
             Values :: [value()],
             Consistency :: consistency(),
             PageSize :: non_neg_integer() | undefined.
aquery(Host, Query, Values, Consistency, PageSize) ->
    ?BACKEND:aquery(Host, Query, Values, Consistency, PageSize).

%% @doc A wrapper around {@link pquery/5}
%% @spec pquery(Host, Query, Consistency) -> {ok, Result :: result()} | {error, Error :: error()}
pquery(Host, Query, Consistency) ->
    pquery(Host, Query, [], Consistency, undefined).

%% @doc A wrapper around {@link pquery/5}
%% @spec pquery(Host, Query, Values, Consistency) -> {ok, Result :: result()} | {error, Error :: error()}
pquery(Host, Query, Values, Consistency) when is_list(Values) ->
    pquery(Host, Query, Values, Consistency, undefined);

pquery(Host, Query, Consistency, PageSize) when is_atom(PageSize) ->
    pquery(Host, Query, [], Consistency, PageSize).

%% @doc Execute a query as a prepared query (in the context of a virtual host).
%% 
%% `Query' is a query string where '?' characters are substituted with 
%% parameters from the `Values' list. 
%% `Host' is the virtual host to execute the query for.
%% 
-spec pquery(Host, Query, Values, Consistency, PageSize) -> {ok, Result :: result()} | {error, Error :: error()} when
             Host :: binary(), % ejabberd:server()
             Query :: binary() | string(), % seestar_session:'query'()
             Values :: [value()],
             Consistency :: consistency(),
             PageSize :: non_neg_integer() | undefined.
pquery(Host, Query, Values, Consistency, PageSize) ->
    ?BACKEND:pquery(Host, Query, Values, Consistency, PageSize).

%% @doc A wrapper around {@link pquery_async/5}
%% @spec pquery_async(Host, Query, Consistency) -> ok
pquery_async(Host, Query, Consistency) ->
    pquery_async(Host, Query, [], Consistency, undefined).

%% @doc A wrapper around {@link pquery_async/5}
%% @spec pquery_async(Host, Query, Values, Consistency) -> ok
pquery_async(Host, Query, Values, Consistency) when is_list(Values) ->
    pquery_async(Host, Query, Values, Consistency, undefined);

pquery_async(Host, Query, Consistency, PageSize) when is_atom(PageSize) ->
    pquery_async(Host, Query, [], Consistency, PageSize).

%% @doc Execute an asynchronous prepared query (in the context of a virtual host).
%% 
%% `Query' is a query string where '?' characters are substituted with 
%% parameters from the `Values' list. 
%% `Host' is the virtual host to execute the query for.
%% 
%% ToDo: The return value and how to signal the result to the caller hasn't 
%% been determined. Needs review and may change in the future.
%% 
-spec pquery_async(Host, Query, Values, Consistency, PageSize) -> ok when
             Host :: binary(),
             Query :: binary() | string(),
             Values :: [value()],
             Consistency :: consistency(),
             PageSize :: non_neg_integer() | undefined.
pquery_async(Host, Query, Values, Consistency, PageSize) ->
    ?BACKEND:pquery_async(Host, Query, Values, Consistency, PageSize).

%% @doc Executes a batch of queries as prepared queries (in the context of a virtual host).
%% 
%% `Queries' is a list of `{Query, Values}' tuples where 
%% `Query' is a query string where '?' characters are substituted with 
%% parameters from the list `Values'. 
%% 
-spec batch_pquery(Host, Queries, Type, Consistency) -> {ok, Result :: seestar_result:result()} | {error, Error :: seestar_error:error()} when
             Host :: binary(),
             Queries :: [{binary() | string(), [value()]}],
             Type :: logged | unlogged | counter,
             Consistency :: consistency().
batch_pquery(Host, Queries, Type, Consistency) ->
    ?BACKEND:batch_pquery(Host, Queries, Type, Consistency).

%% @doc Extracts rows from a query result
%%
%% Returns a list of rows. Each row is a list of values.
%%
-spec rows(Result :: rows_result()) -> [[value()]].
rows(Result) ->
    ?BACKEND:rows(Result).

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
    cassandra:single_result(Result) /= <<0>>.

%% @doc Uses cassandra to generate a version 1 UUID
%% 
%% `Host' is the virtual host to execute the query for.
%% This requires that the table `uuidgen' exists (and is not empty)
%% 
-spec uuid1(Host :: binary()) -> binary().
uuid1(Host) ->
    {ok, Result} = pquery(Host, <<"SELECT NOW() from uuidgen">>, one),
    single_result(Result).

%% @doc Uses cassandra to generate a version 4 UUID
%% 
%% `Host' is the virtual host to execute the query for.
%% This requires that the table `uuidgen' exists (and is not empty)
%% 
-spec uuid4(Host :: binary()) -> binary().
uuid4(Host) ->
    {ok, Result} = pquery(Host, <<"SELECT UUID() from uuidgen">>, one),
    single_result(Result).

%% @doc An alias of {@link uuid1/1}
-spec timeuuid(Host :: binary()) -> binary().
timeuuid(Host) ->
    uuid1(Host).

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
