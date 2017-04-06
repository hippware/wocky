%%% @copyright 2016+ Hippware, Inc.
%%% @doc Module to handle token related IQs
-module(mod_wocky_token).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include("wocky.hrl").

%% gen_mod behaviour
-behaviour(gen_mod).
-export([start/2, stop/1]).

%% IQ handler callback
-export([handle_iq/3]).

%%%===================================================================
%%% gen_mod implementation
%%%===================================================================

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_TOKEN,
                                  ?MODULE, handle_iq, IQDisc).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_TOKEN).


%%%===================================================================
%%% IQ handler callback
%%%===================================================================

handle_iq(From, _To, #iq{sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer, lresource = LResource} = From,
    case lists:member(LServer, ?MYHOSTS) of
        true ->
            handle_local_iq(LUser, LServer, LResource, IQ);

        _ ->
            IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]}
    end.

handle_local_iq(LUser, _LServer, LResource, #iq{type = get} = IQ) ->
    {ok, {Token, Expiry}} = ?wocky_token:assign(LUser, LResource),
    iq_result(IQ,
              [{<<"expiry">>, expiry_string(Expiry)}],
              [#xmlcdata{content = Token}]);
handle_local_iq(LUser, _LServer, LResource, #iq{type = set} = IQ) ->
    ok = ?wocky_token:release(LUser, LResource),
    iq_result(IQ, [], []).

iq_result(IQ, Attrs, Content) ->
    IQ#iq{type = result,
          sub_el = [#xmlel{name = <<"query">>,
                           attrs = [{<<"xmlns">>, ?NS_TOKEN} | Attrs],
                           children = Content}]}.

expiry_string(Expiry) ->
    wocky_db:timestamp_to_string(Expiry).
