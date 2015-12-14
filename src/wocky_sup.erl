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

    %% Example child supervisor spec
    %% ChildSup = #{id       => child_sup,
    %%              start    => {child_sup, start_link, []},
    %%              restart  => permanent,
    %%              shutdown => infinity,
    %%              type     => supervisor,
    %%              modules  => [child_sup]},

    {ok, {SupFlags, []}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
