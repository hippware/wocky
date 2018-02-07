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

    BotExpiryMon = #{id       => wocky_bot_expiry_mon,
                     start    => {wocky_bot_expiry_mon, start_link, []},
                     restart  => permanent,
                     shutdown => 5000,
                     type     => worker,
                     modules  => [wocky_bot_expiry_mon]},

    ExploreNearbyWorkerSup =
    #{id       => wocky_explore_worker_sup,
      start    => {wocky_explore_worker_sup, start_link, []},
      restart  => permanent,
      shutdown => 5000,
      type     => supervisor,
      modules  => []},

    WatcherClient = #{id    => ?wocky_watcher_client,
                      start => {?wocky_watcher_client, start_link, []}},

    {ok, {SupFlags, [BotExpiryMon,
                     ExploreNearbyWorkerSup,
                     WatcherClient
                    ]}}.
