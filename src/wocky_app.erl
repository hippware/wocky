%%% @copyright 2015+ Hippware, Inc.
%%% @doc Wocky application module
-module(wocky_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start/0, stop/0]).


start() ->
    {ok, _} = application:ensure_all_started(wocky),
    ok = application:ensure_started(wocky).

stop() ->
    application:stop(wocky).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = wocky_sup:start_link(),

    % Hack to deal with the fact that, for the moment, we're using a persistant
    % store for our session managment. That means that when the last node is
    % torn down in a cluster, there's nothing to clean up its sessions. In MIM
    % this isn't a problem because the session store is RAM-backed so goes away
    % anyway. With C*, though, we need to make sure we clean up before starting
    % so that we're not left with stale sessions:
  %  ejabberd_sm_wocky:cleanup(node()),

    StartEJD = application:get_env(wocky, start_ejabberd, false),
    ok = maybe_start_ejabberd(StartEJD),

    {ok, Pid}.

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

maybe_start_ejabberd(true)  -> ejabberd:start();
maybe_start_ejabberd(false) -> ok.
