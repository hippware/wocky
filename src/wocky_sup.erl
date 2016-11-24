%%% @copyright 2015+ Hippware, Inc.
%%% @doc Top level wocky supervisor
-module(wocky_sup).

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

    UserIdx = #{id       => wocky_index,
                start    => {wocky_index, start_link, []},
                restart  => permanent,
                shutdown => 5000,
                type     => worker,
                modules  => [wocky_index]},

    BotExpiryMon = #{id       => wocky_bot_expiry_mon,
                     start    => {wocky_bot_expiry_mon, start_link, []},
                     restart  => permanent,
                     shutdown => 5000,
                     type     => worker,
                     modules  => [wocky_bot_expiry_mon]},

    {ok, {SupFlags, [UserIdx, BotExpiryMon]}}.
