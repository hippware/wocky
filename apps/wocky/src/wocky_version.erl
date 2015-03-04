%%%----------------------------------------------------------------------
%%% File    : wocky_version.hrl
%%% Author  : Beng Tan
%%% Purpose : A sample implementation of XEP-0092: Software Version
%%%
%%%
%%% Copyright (C) 2015 Hippware
%%%----------------------------------------------------------------------

-module(wocky_version).

-behaviour(gen_mod).

-export([start/2, 
         stop/1,
         process_local_iq/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("wocky.hrl").

-spec start(ejabberd:server(), list()) -> ok.
start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),

    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
        ?NS_VERSION, ?MODULE, process_local_iq, IQDisc).

-spec stop(ejabberd:server()) -> ok.
stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
        ?NS_VERSION).

-spec process_local_iq(ejabberd:jid(), ejabberd:jid(), ejabberd:iq()) -> ejabberd:iq().
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
                            #xmlel{name = <<"name">>, children = [{xmlcdata, <<"wocky">>}]},
                            #xmlel{name = <<"version">>, children = [{xmlcdata, ?WOCKY_VERSION}]}
                        ]}]}
    end.
