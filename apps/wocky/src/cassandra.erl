%%%----------------------------------------------------------------------
%%% File    : cassandra.erl
%%% Author  : Beng Tan
%%% Purpose : Cassandra dispatcher 
%%% Copyright (c) 2015 Hippware
%%%
%%% This initialises, and dispatches to, a configurable backend. 
%%% The backend interfaces with the Cassandra driver.
%%% 
%%% This is implemented as an ejabberd module. 
%%% To use it, add this to the modules list in ejabberd.cfg (preferably first):
%%%
%%% {cassandra, [{backend, <backend>}]},
%%%
%%% Other values can be added to the module options list. They are
%%% backend-specific and will be passed as-is to the backend module.
%%%
%%%
%%% Whilst this is supposed to be a generic Cassandra interface, for practical
%%% purposes, it is still closely coupled to the backend. If we ever support 
%%% more than one backend, then the interface needs to be made backend agnostic.
%%%
%%%
%%% The other reason for this module is to reduce verbosity. 
%%% It's nicer to write cassandra:func_1() rather than cassandra_seestar:func_1().
%%%----------------------------------------------------------------------

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
         prepare_query/2,
         pquery/3, pquery/4, pquery/5,
         pquery_async/3, pquery_async/4, pquery_async/5,
         rows/1, single_result/1,
         uuid1/1, uuid4/1, timeuuid/1]).

%% gen_mod
-behaviour(gen_mod).
-export([start/2, stop/1]).

%% gen_mod callbacks
start(Host, Opts) ->
    start_backend_module(Opts),
    ?BACKEND:start(Host, Opts).

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

aquery(Host, Query, Consistency) ->
    aquery(Host, Query, [], Consistency).

aquery(Host, Query, Values, Consistency) when is_list(Values) ->
    aquery(Host, Query, Values, Consistency, undefined);

aquery(Host, Query, Consistency, PageSize) when is_atom(PageSize) ->
    aquery(Host, Query, [], Consistency, PageSize).

aquery(Host, Query, Values, Consistency, PageSize) ->
    ?BACKEND:aquery(Host, Query, Values, Consistency, PageSize).

prepare_query(Host, Query) ->
    ?BACKEND:prepare_query(Host, Query).

pquery(Host, Query, Consistency) ->
    pquery(Host, Query, [], Consistency).

pquery(Host, Query, Values, Consistency) when is_list(Values) ->
    pquery(Host, Query, Values, Consistency, undefined);

pquery(Host, Query, Consistency, PageSize) when is_atom(PageSize) ->
    pquery(Host, Query, [], Consistency, PageSize).

-spec pquery(binary(), % ejabberd:server(),
             binary() | string(), % seestar_session:'query'()
             [value()],
             consistency(),
             non_neg_integer() | undefined) ->
        {ok, Result :: result()} | {error, Error :: error()}.
pquery(Host, Query, Values, Consistency, PageSize) ->
    ?BACKEND:pquery(Host, Query, Values, Consistency, PageSize).

pquery_async(Host, Query, Consistency) ->
    pquery_async(Host, Query, [], Consistency).

pquery_async(Host, Query, Values, Consistency) when is_list(Values) ->
    pquery_async(Host, Query, Values, Consistency, undefined);

pquery_async(Host, Query, Consistency, PageSize) when is_atom(PageSize) ->
    pquery_async(Host, Query, [], Consistency, PageSize).

pquery_async(Host, Query, Values, Consistency, PageSize) ->
    ?BACKEND:pquery_async(Host, Query, Values, Consistency, PageSize).

rows(Result) ->
    ?BACKEND:rows(Result).

single_result(Result) ->
    [[Value | _] | _]= ?BACKEND:rows(Result),
    Value.

uuid1(Host) ->
    {ok, Result} = pquery(Host, <<"SELECT NOW() from uuidgen">>, one),
    [[Value | _] | _]= rows(Result),
    Value.

uuid4(Host) ->
    {ok, Result} = pquery(Host, <<"SELECT UUID() from uuidgen">>, one),
    [[Value | _] | _]= rows(Result),
    Value.

timeuuid(Host) ->
    uuid1(Host).

