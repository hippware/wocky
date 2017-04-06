%%% @copyright 2015+ Hippware, Inc.
%%% @doc Wocky application module
-module(wocky_xmpp_app).

-include("wocky.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start/1, start/0, stop/0, ensure_loaded/1,
         server/0, is_testing/0,
         get_config/1, get_config/2]).


-spec start(string()) -> ok.
start(InstName) ->
    ok = ensure_loaded(wocky_xmpp),
    [Inst, Env] = string:tokens(InstName, "."),
    ok = application:set_env(wocky_xmpp, wocky_inst, Inst),
    ok = application:set_env(wocky_xmpp, wocky_env, Env),
    start().

-spec start() -> ok.
start() ->
    {ok, _} = application:ensure_all_started(wocky_xmpp),
    ok.

-spec stop() -> ok | {error, term()}.
stop() ->
    _ = ejabberd:stop(),
    application:stop(wocky_xmpp).

-spec ensure_loaded(atom()) -> ok | {error, term()}.
ensure_loaded(App) ->
    case application:load(App) of
        ok -> ok;
        {error, {already_loaded, _}} -> ok;
        {error, _} = Error -> Error
    end.

-spec version() -> binary().
version() ->
    element(2, application:get_key(wocky_xmpp, vsn)).

-spec server() -> ejabberd:server().
server() ->
    hd(servers()).

-spec servers() -> [ejabberd:server()].
servers() ->
    application:get_env(wocky_xmpp, server_names, [<<"localhost">>]).

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
        case ejabberd_config:get_local_option(Key, get_host()) of
            undefined -> default_config(Key, Default);
            Value -> Value
        end
    catch
        _:_ -> default_config(Key, Default)
    end.

get_host() ->
    hd(ejabberd_config:get_global_option(hosts)).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    Minimal = get_minimal_mode(),
    ok = maybe_change_loglevel(Minimal),

    {ok, Inst} = set_wocky_inst(),

    {ok, CfgDir} = application:get_env(wocky_xmpp, config_dir),
    CfgPath = filename:join(CfgDir, Inst ++ ".cfg"),
    {ok, CfgTerms} = file:consult(CfgPath),

    ok = cache_server_names(CfgTerms),

    ok = maybe_enable_notifications(CfgTerms),

    ok = ensure_loaded(ejabberd),
    ok = application:set_env(ejabberd, config, CfgPath),
    ok = maybe_start_ejabberd(not Minimal),

    wocky_sup:start_link().

stop(_State) ->
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

set_wocky_inst() ->
    Env = get_wocky_env(),
    Inst = get_wocky_inst(),
    InstName = lists:flatten([Inst, ".", Env]),

    ok = lager:info("Wocky instance ~s starting with version ~s.",
                    [InstName, version()]),

    ok = application:set_env(wocky_xmpp, wocky_env, Env),
    ok = application:set_env(wocky_xmpp, wocky_inst, Inst),
    ok = application:set_env(wocky_xmpp, instance_name, InstName),
    {ok, InstName}.

get_wocky_env() ->
    case os:getenv("WOCKY_ENV") of
        false ->
            case os:getenv("MIX_ENV") of
                false ->
                    {ok, Value} = application:get_env(wocky_xmpp, wocky_env),
                    Value;

                Value ->
                    Value
            end;

        Value ->
            Value
    end.

get_wocky_inst() ->
    case os:getenv("WOCKY_INST") of
        false ->
            {ok, Value} = application:get_env(wocky_xmpp, wocky_inst),
            Value;

        Value ->
            Value
    end.

get_minimal_mode() ->
    case os:getenv("WOCKY_MINIMAL") of
        false -> false;
        _ -> true
    end.

maybe_change_loglevel(true) ->
    lager:set_loglevel(lager_console_backend, warning);
maybe_change_loglevel(_) ->
    ok.

is_testing_server(<<"localhost">>) -> true;
is_testing_server(<<"testing.", _/binary>>) -> true;
is_testing_server(_) -> false.

default_config(Key, Default) ->
    application:get_env(wocky_xmpp, Key, Default).

cache_server_names(CfgTerms) ->
    Servers = proplists:get_value(hosts, CfgTerms),
    BinServers = lists:map(fun (S) -> iolist_to_binary(S) end, Servers),
    application:set_env(wocky_xmpp, server_names, BinServers).

maybe_enable_notifications(CfgTerms) ->
    NotificationHandlerModule =
    case proplists:get_value(notification_system, CfgTerms, none) of
        aws ->
            ok = lager:info("AWS Notifications enabled"),
            'Elixir.Wocky.PushNotifier.SNS';
        test ->
            ok = lager:info("Notification testing system enabled"),
            'Elixir.Wocky.PushNotifier.Test';
        none ->
            ok = lager:info("Notifications disabled"),
            'Elixir.Wocky.PushNotifier.Null'
    end,
    wocky_notification_handler:set_handler(NotificationHandlerModule).

maybe_start_ejabberd(true) ->
    {ok, _} = ejabberd:start(),
    ok;
maybe_start_ejabberd(_) ->
    ok.
