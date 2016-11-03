%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module to test MIM callback crash handling
%%%
-module(mod_wocky_crash_test).

-behaviour(gen_mod).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include("wocky.hrl").

%% gen_mod handlers
-export([start/2, stop/1]).

%% IQ hook
-export([handle_iq/3]).

%%%===================================================================
%%% gen_mod handlers
%%%===================================================================

start(Host, _Opts) ->
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_CRASH_TEST,
                                  ?MODULE, handle_iq, parallel),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_CRASH_TEST,
                                  ?MODULE, handle_iq, parallel).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_CRASH_TEST),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_CRASH_TEST).

-spec handle_iq(From :: ejabberd:jid(),
                To :: ejabberd:jid(),
                IQ :: iq()) -> iq().
handle_iq(_From, _To, _IQ) ->
    %% Simple way to generate a crash without tipping off the comiler:
    lists:last([]).

