%%% @copyright 2015+ Hippware, Inc.
%%% @doc Wocky application module
-module(wocky_xmpp_app).

-include("wocky.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start/1, start/0, stop/0, ensure_loaded/1,
         server/0, is_testing/0]).

-define(system, 'Elixir.System').
-define(confex, 'Elixir.Confex').


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
    hd(ejabberd_config:get_global_option(hosts)).

-spec is_testing() -> boolean().
is_testing() ->
    is_testing_server(server()).


%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    Env = get_wocky_env(),
    Inst = get_wocky_inst(),
    InstName = iolist_to_binary([Inst, ".", Env]),

    ok = lager:info("Wocky instance ~s starting with version ~s.",
                    [InstName, version()]),

    ok = mod_wocky_access:init(),
    ok = mod_wocky_publishing:init(),

    {ok, CfgPath} = load_xmpp_config(),
    ok = start_ejabberd(CfgPath),

    wocky_sup:start_link().

stop(_State) ->
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

get_wocky_env() ->
    application:get_env(wocky_xmpp, wocky_env, nil).

get_wocky_inst() ->
    ?confex:get(wocky_xmpp, wocky_inst).

load_xmpp_config() ->
    CfgTplPath = filename:join(code:priv_dir(wocky_xmpp), "ejabberd.cfg"),
    {ok, CfgTplTerms} = file:consult(CfgTplPath),

    CfgTerms = [{odbc_server, db_config()} | ?confex:process_env(CfgTplTerms)],

    TmpDir = 'Elixir.System':tmp_dir(),
    CfgPath = filename:join(TmpDir, "ejabberd.cfg"),
    ok = write_terms(CfgPath, CfgTerms),
    {ok, CfgPath}.

db_config() ->
    DbConfig = ?wocky_repo:config(),
    {pgsql,
     proplists:get_value(hostname, DbConfig),
     proplists:get_value(port,     DbConfig),
     proplists:get_value(database, DbConfig),
     proplists:get_value(username, DbConfig),
     proplists:get_value(password, DbConfig)}.

write_terms(Filename, List) ->
    Format = fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
    Text = lists:map(Format, List),
    file:write_file(Filename, Text).

start_ejabberd(CfgPath) ->
    ok = ensure_loaded(ejabberd),
    ok = application:set_env(ejabberd, config, CfgPath),
    {ok, _} = application:ensure_all_started(ejabberd),
    ok.

is_testing_server(<<"localhost">>) -> true;
is_testing_server(<<"testing.", _/binary>>) -> true;
is_testing_server(_) -> false.
