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

    %% Try to configure wocky using settings in the application environment
    ok = wocky_db:maybe_configure(),

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
