%%% @copyright 2017+ Hippware, Inc.
%%%
%%% @doc Module implementing Wocky Cron tasks
%%%
-module(wocky_cron).

-behaviour(supervisor).

-compile({parse_transform, cut}).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-ignore_xref([start_link/0]).

%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    SupFlags = #{strategy  => one_for_one,
                 intensity => 1,
                 period    => 5},

    Jobs = [#{id       => T,
              start    => {crone, start_one, [T]},
              restart  => permanent,
              shutdown => 5000,
              type     => worker,
              modules  => [crone]}
            || T <- tasks()],

    {ok, {SupFlags, Jobs}}.

%%%===================================================================
%%% Helpers
%%%===================================================================

tasks() ->
    application:get_env(crone, tasks, []).
