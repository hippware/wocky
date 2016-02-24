%%% @copyright 2015+ Hippware, Inc.
%%% @doc Wocky application module
-module(wocky_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start/1, start/0, stop/0,
         start_ejabberd/0, start_ejabberd/1]).


-spec start(string()) -> ok.
start(Env) ->
    ok = application:set_env(wocky, wocky_env, Env),
    start().

-spec start() -> ok.
start() ->
    {ok, _} = application:ensure_all_started(wocky),
    ok.

-spec stop() -> ok | {error, term()}.
stop() ->
    _ = ejabberd:stop(),
    application:stop(wocky).

-spec start_ejabberd() -> ok.
start_ejabberd() ->
    {ok, CfgDir} = application:get_env(wocky, config_dir),
    start_ejabberd(CfgDir).

-spec start_ejabberd(string()) -> ok.
start_ejabberd(CfgDir) ->
    {ok, Env} = application:get_env(wocky, wocky_env),
    CfgPath = filename:join(CfgDir, Env ++ ".cfg"),

    _ = application:load(ejabberd),
    ok = application:set_env(ejabberd, config, CfgPath),
    ejabberd:start().


%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    ok = set_wocky_env(),
    {ok, Pid} = wocky_sup:start_link(),
    ok = maybe_start_ejabberd(),
    {ok, Pid}.

stop(_State) ->
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

set_wocky_env() ->
    Env = case os:getenv("WOCKY_ENV") of
              false ->
                  {ok, Value} = application:get_env(wocky, wocky_env),
                  Value;

              Value ->
                  Value
          end,
    lager:info("Wocky starting in the '~s' environment.", [Env]),
    application:set_env(wocky, wocky_env, Env).

maybe_start_ejabberd() ->
    {ok, StartEJD} = application:get_env(wocky, start_ejabberd),
    maybe_start_ejabberd(StartEJD).

maybe_start_ejabberd(true)  -> start_ejabberd();
maybe_start_ejabberd(false) -> ok.
