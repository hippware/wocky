%%% @copyright 2015+ Hippware, Inc.
%%% @doc A sample implementation of XEP-0092: Software Version
%%%
%%% This is more of an example rather than something intended to be used
%%% in production.

-module(mod_wocky_version).

-behaviour(gen_mod).

-export([start/2,
         stop/1,
         process_local_iq/3]).

-ignore_xref([{process_local_iq, 3}]).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include("wocky.hrl").

%% @doc Start the module
-spec start(ejabberd:server(), list()) -> ok.
start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),

    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_VERSION, ?MODULE,
                                  process_local_iq, IQDisc).

%% @doc Stop the module
-spec stop(ejabberd:server()) -> ok.
stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_VERSION).

%% @doc Process the iq stanza
-spec process_local_iq(ejabberd:jid(), ejabberd:jid(), ejabberd:iq())
                      -> ejabberd:iq().
process_local_iq(_From, _To,
    #iq{type = Type, sub_el = SubEl} = IQ) ->
    case Type of
        set ->
            IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
        get ->
            IQ#iq{type = result,
                sub_el = [
                    #xmlel{name = <<"query">>,
                        attrs = [{<<"xmlns">>, ?NS_VERSION}],
                        children=[
                            #xmlel{name = <<"name">>,
                                   children = [{xmlcdata, <<"wocky">>}]},
                            #xmlel{name = <<"version">>,
                                   children = [{xmlcdata, wocky_app:version()}]}
                        ]}]}
    end.
