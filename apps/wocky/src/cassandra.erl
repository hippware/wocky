%%%----------------------------------------------------------------------
%%% File    : cassandra.erl
%%% Author  : Beng Tan
%%% Purpose : Cassandra dispatcher 
%%%
%%% This initialises, and dispatches to, a configurable backend. 
%%% The backend interfaces with the Cassandra driver.
%%% 
%%% This is implemented as an ejabberd module. 
%%%
%%% To use it, add this to the modules list in ejabberd.cfg (preferably first):
%%%
%%% {cassandra, [{backend, <backend>}]},
%%%
%%% Other values can be added to the module options list. They are
%%% backend-specific and will be passed as-is to the backend module.
%%%
%%%
%%% The other reason for this module is to reduce verbosity. 
%%% It's nicer to write cassandra:func_1() rather than cassandra_seestar:func_1().
%%%
%%% Copyright (C) 2015 Hippware
%%%----------------------------------------------------------------------

-module(cassandra).

-define(BACKEND, (cassandra_backend:backend())).

%% API
-export([aquery/3, aquery/4, aquery/5, 
         pquery/3, pquery/4, pquery/5]).

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

pquery(Host, Query, Consistency) ->
    pquery(Host, Query, [], Consistency).

pquery(Host, Query, Values, Consistency) when is_list(Values) ->
    pquery(Host, Query, Values, Consistency, undefined);

pquery(Host, Query, Consistency, PageSize) when is_atom(PageSize) ->
    pquery(Host, Query, [], Consistency, PageSize).

pquery(Host, Query, Values, Consistency, PageSize) ->
    ?BACKEND:pquery(Host, Query, Values, Consistency, PageSize).
