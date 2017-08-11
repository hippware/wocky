-module(wocky_explore_worker_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one},

    ChildSpec = #{id => worker,
                  start => {wocky_explore_worker, start_link, []},
                  restart => temporary
                 },

    {ok, {SupFlags, [ChildSpec]}}.
