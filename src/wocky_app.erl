%%% @copyright 2015+ Hippware, Inc.
%%% @doc Wocky application module
-module(wocky_app).

-include("wocky.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start/1, start/0, stop/0, ensure_loaded/1,
         version/0, server/0, servers/0, is_testing/0,
         get_config/1, get_config/2]).

-ignore_xref([{start, 0}, {start, 1}, {stop, 0}]).


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

-spec ensure_loaded(atom()) -> ok | {error, term()}.
ensure_loaded(App) ->
    case application:load(App) of
        ok -> ok;
        {error, {already_loaded, _}} -> ok;
        {error, _} = Error -> Error
    end.

-spec version() -> binary().
version() ->
    element(2, application:get_key(wocky, vsn)).

-spec server() -> binary().
server() ->
    hd(servers()).

-spec servers() -> [binary()].
servers() ->
    application:get_env(wocky, server_names, [<<"localhost">>]).

-spec is_testing() -> boolean().
is_testing() ->
    is_testing_server(server()).

-spec get_config(atom()) -> term().
get_config(Key) ->
    get_config(Key, undefined).

-spec get_config(atom(), term()) -> term().
get_config(Key, Default) ->
    try
        %% Try pulling the config from ejabberd
        Host = hd(ejabberd_config:get_global_option(hosts)),
        case ejabberd_config:get_local_option(Key, Host) of
            undefined -> default_config(Key, Default);
            Value -> Value
        end
    catch
        _:_ -> default_config(Key, Default)
    end.


%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    {ok, Env} = set_wocky_env(),

    {ok, CfgDir} = application:get_env(wocky, config_dir),
    CfgPath = filename:join(CfgDir, Env ++ ".cfg"),
    {ok, CfgTerms} = file:consult(CfgPath),

    ok = configure_db(CfgTerms),
    {ok, _} = application:ensure_all_started(schemata),

    ok = cache_server_names(CfgTerms),

    ok = ensure_loaded(ejabberd),
    ok = application:set_env(ejabberd, config, CfgPath),
    case os:getenv("WOCKY_MINIMAL") of
        false ->
            {ok, _} = ejabberd:start(),
            ok;
        _ ->
            ok
    end,

    wocky_sup:start_link().

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
    ok = lager:info("Wocky ~s starting in the '~s' environment.",
                    [version(), Env]),
    ok = application:set_env(wocky, wocky_env, Env),
    {ok, Env}.

is_testing_server(<<"localhost">>) -> true;
is_testing_server(<<"testing.", _/binary>>) -> true;
is_testing_server(_) -> false.

default_config(Key, Default) ->
    application:get_env(wocky, Key, Default).

configure_db(CfgTerms) ->
    ok = ensure_loaded(schemata),
    Cluster = proplists:get_value(schemata_cluster, CfgTerms),
    apply_db_config(Cluster).

apply_db_config(undefined) -> ok;
apply_db_config(Cluster) -> application:set_env(schemata, cluster, Cluster).

cache_server_names(CfgTerms) ->
    Servers = proplists:get_value(hosts, CfgTerms),
    BinServers = lists:map(fun (S) -> iolist_to_binary(S) end, Servers),
    application:set_env(wocky, server_names, BinServers).
