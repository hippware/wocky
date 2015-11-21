%%% @copyright 2015+ Hippware, Inc.
%%% @doc Wocky application module
-module(wocky_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start/0]).


start() ->
    application:start(wocky).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = wocky_sup:start_link(),

    {ok, Backend} = application:get_env(wocky, backend),
    ok = cassandra:start_backend(Backend),

    %% Try to configure wocky using settings in the application environment
    ok = cassandra:maybe_configure(),

    case application:get_env(wocky, start_ejabberd, false) of
        true  -> ejabberd:start();
        false -> ok
    end,

    {ok, Pid}.

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

