%%% @copyright 2015+ Hippware, Inc.
%%% @doc Top level wocky supervisor
-module(wocky_sup).

-include("wocky.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    SupFlags = #{strategy  => one_for_one,
                 intensity => 1,
                 period    => 5},

    ExploreNearbyWorkerSup =
    #{id       => wocky_explore_worker_sup,
      start    => {wocky_explore_worker_sup, start_link, []},
      restart  => permanent,
      shutdown => 5000,
      type     => supervisor,
      modules  => []},

    {ok, {SupFlags, [ExploreNearbyWorkerSup]}}.
