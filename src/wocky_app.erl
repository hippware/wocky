%%% @copyright 2015+ Hippware, Inc.
%%% @doc Wocky application module
-module(wocky_app).

-include("wocky.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start/1, start/0, stop/0,
         start_ejabberd/0, start_ejabberd/1,
         version/0]).


-spec start(string()) -> ok.
start(Env) ->
    ok = ensure_loaded(wocky),
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

    ok = ensure_loaded(ejabberd),
    ok = application:set_env(ejabberd, config, CfgPath),
    {ok, _} = ejabberd:start(),
    ok.

version() ->
    ?WOCKY_VERSION.


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

ensure_loaded(App) ->
    case application:load(App) of
        ok -> ok;
        {error, {already_loaded, _}} -> ok;
        {error, _} = Error -> Error
    end.

set_wocky_env() ->
    Env = case os:getenv("WOCKY_ENV") of
              false ->
                  {ok, Value} = application:get_env(wocky, wocky_env),
                  Value;

              Value ->
                  Value
          end,
    ok = lager:info("Wocky ~s starting in the '~s' environment.",
                    [version(), Env]),
    application:set_env(wocky, wocky_env, Env).

maybe_start_ejabberd() ->
    {ok, StartEJD} = application:get_env(wocky, start_ejabberd),
    maybe_start_ejabberd(StartEJD).

maybe_start_ejabberd(true)  -> start_ejabberd();
maybe_start_ejabberd(false) -> ok.
