%%%----------------------------------------------------------------------
%%% File    : cassandra_seestar_sup.erl
%%% Author  : Beng Tan
%%% Purpose : Supervisor for cassandra_seestar.erl
%%%
%%% Copyright (C) 2015 Hippware
%%%----------------------------------------------------------------------

-module(cassandra_seestar_sup).

-behaviour(supervisor).
-export([start/2,
         stop/1,
         start_link/2, 
         init/1]).

start(Host, Servers) ->
    supervisor:start_child(ejabberd_sup, supervisor_spec(Host, Servers)).

stop(Host) ->
    Tag = {?MODULE, Host},
    supervisor:terminate_child(ejabberd_sup, Tag),
    supervisor:delete_child(ejabberd_sup, Tag).

start_link(Host, Servers) ->
    supervisor2:start_link({local, ?MODULE}, ?MODULE, [Host, Servers]).

supervisor_spec(Host, Servers) ->
    {{?MODULE, Host},
     {?MODULE, start_link, [Host, Servers]},
     permanent,
     infinity,
     supervisor,
     [?MODULE]}.

worker_spec(Host, Opts, WorkerNumber) ->
    Server = lists:keystore(worker, 1, Opts, {worker, WorkerNumber}),
    {{Host, {proplists:get_value(server, Server), proplists:get_value(port, Server)}, WorkerNumber},
     {cassandra_seestar, start_link, [Host, Server]},
     {permanent, 10}, %% Delay is 10 seconds
     infinity,
     worker,
     [cassandra_seestar]}.

worker_specs(Host, Servers) ->
    [worker_spec(Host, Server, WorkerNumber)
     || Server <- Servers,
        WorkerNumber <- lists:seq(1, proplists:get_value(workers, Server, 1))].

init([Host, Servers]) ->
    Specs = worker_specs(Host, Servers),
    {ok, {{one_for_one, 10, 1}, Specs}}.
