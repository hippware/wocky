%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module adding Honeybadger context
%%% See https://honeybadger.io
%%%

-module(mod_wocky_honeybadger).

-include("wocky.hrl").

-behaviour(gen_mod).

%% gen_mod handlers
-export([start/2, stop/1]).

-define(HOOK_PRIORITY, 5).
-define(honeybadger, 'Elixir.Wocky.Honeybadger').


%%%===================================================================
%%% gen_mod handlers
%%%===================================================================

start(Host, _Opts) ->
    ejabberd_hooks:add(sm_register_connection_hook, Host,
                       fun sm_register_connection_hook/3,
                       ?HOOK_PRIORITY),
    ejabberd_hooks:add(filter_local_packet, Host,
                       fun filter_local_packet_hook/1,
                       ?HOOK_PRIORITY),
    ejabberd_hooks:add(iq_handler_crash, Host,
                       fun iq_handler_crash_hook/4,
                       ?HOOK_PRIORITY).

stop(Host) ->
    ejabberd_hooks:delete(iq_handler_crash, Host,
                          fun iq_handler_crash_hook/4,
                          ?HOOK_PRIORITY),
    ejabberd_hooks:delete(filter_local_packet, Host,
                          fun filter_local_packet_hook/1,
                          ?HOOK_PRIORITY),
    ejabberd_hooks:delete(sm_register_connection_hook, Host,
                          fun sm_register_connection_hook/3,
                          ?HOOK_PRIORITY).

%%%===================================================================
%%% User connection handler
%%%===================================================================

-spec sm_register_connection_hook(ejabberd_sm:sid(), ejabberd:jid(),
                                  any()) -> ok.
sm_register_connection_hook(SID, JID, Info) ->
    ?honeybadger:context(#{'connection.JID'  => jid:to_binary(JID),
                           'connection.SID'  => to_string(SID),
                           'connection.info' => to_string(Info)}).

%%%===================================================================
%%% Incoming packet handler
%%%===================================================================

-type filter_packet() :: {ejabberd:jid(), ejabberd:jid(), jlib:xmlel()}.
-spec filter_local_packet_hook(filter_packet() | drop) ->
    filter_packet().

%% Packets to the bot - dropped if they were processed here.
filter_local_packet_hook(P = {From, To, Packet}) ->
    ok = ?honeybadger:context(#{'last_packet.from'   => jid:to_binary(From),
                                'last_packet.to'     => jid:to_binary(To),
                                'last_packet.packet' => to_string(Packet)}),
    P;
filter_local_packet_hook(drop) -> drop.

%%%===================================================================
%%% IQ handler crash
%%%===================================================================

-spec iq_handler_crash_hook(ejabberd:jid(), ejabberd:jid(), ejabberd:iq(),
                            any()) -> ok.
iq_handler_crash_hook(From, To, IQ, _Exception) ->
    ?honeybadger:notify(
       <<"IQ handler crash">>,
       #{'iq_crash.from'       => jid:to_binary(From),
         'iq_crash.to'         => jid:to_binary(To),
         'iq_crash.iq.id'      => IQ#iq.id,
         'iq_crash.iq.type'    => atom_to_binary(IQ#iq.type, utf8),
         'iq_crash.iq.ns'      => IQ#iq.xmlns,
         'iq_crash.iq.payload' => exml:to_binary(IQ#iq.sub_el)
        },
       erlang:get_stacktrace()).

%%%===================================================================
%%% Private helpers
%%%===================================================================

to_string(Term) ->
    list_to_binary(io_lib:fwrite("~p", [Term])).
