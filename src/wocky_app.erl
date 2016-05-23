%%% @copyright 2015+ Hippware, Inc.
%%% @doc Wocky application module
-module(wocky_app).

-include("wocky.hrl").

-behaviour(application).
-behaviour(erld_app).

%% Application callbacks
-export([start/2, stop/1]).
-export([start/1, start/0, stop/0,
         start_ejabberd/0, start_ejabberd/1,
         version/0, server/0, is_testing/0,
         get_config/1, bake_cookie/0]).

-ignore_xref([{start, 0},
              {start, 1},
              {start_ejabberd, 0},
              {start_ejabberd, 1},
              {stop, 0}]).


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
    ok = reset_log_levels(),
    ok.

-spec version() -> binary().
version() ->
    element(2, application:get_key(wocky, vsn)).

-spec server() -> binary().
server() ->
    try
        hd(ejabberd_config:get_global_option(hosts))
    catch
        _:_ ->
            <<"localhost">>
    end.

-spec is_testing() -> boolean().
is_testing() ->
    is_testing_server(server()).

-spec get_config(atom()) -> term().
get_config(Key) ->
    %% Try pulling the config from ejabberd
    try
        Host = hd(ejabberd_config:get_global_option(hosts)),
        case ejabberd_config:get_local_option(Key, Host) of
            undefined -> default_config(Key);
            Value -> Value
        end
    catch
        _:_ ->
            default_config(Key)
    end.

bake_cookie() ->
    'ejabberd'.

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    ok = set_wocky_env(),
    {ok, Pid} = wocky_sup:start_link(),
    ok = maybe_start_ejabberd(),
    erld:detach(),
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

is_testing_server(<<"localhost">>) -> true;
is_testing_server(<<"testing.", _/binary>>) -> true;
is_testing_server(_) -> false.

%% ejabberd_app forces the level for ALL Lager backends to 'info'.
%% Reset any backends that need to be running at a different level.
reset_log_levels() ->
    ok = lager:set_loglevel(lager_file_backend, "debug.log", debug),
    ok = lager:set_loglevel(lager_file_backend, "wocky.log", warning),
    ok.

default_config(Key) ->
    {ok, Value} = application:get_env(wocky, Key),
    Value.
