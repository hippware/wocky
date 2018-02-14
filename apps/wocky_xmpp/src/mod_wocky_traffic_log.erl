%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing traffic logging
%%%
-module(mod_wocky_traffic_log).

-include("wocky.hrl").

-behaviour(gen_mod).

%% gen_mod handlers
-export([start/2, stop/1]).

-define(DEFAULT_EXPIRE, 3600).


%%%===================================================================
%%% gen_mod handlers
%%%===================================================================

start(Host, Opts) ->
    wocky_util:set_config_from_opt(expire, traffic_log_expire,
                                   ?DEFAULT_EXPIRE, Opts),
    ejabberd_hooks:add(stanza_sent, Host,
                       fun stanza_sent_hook/4, 80),
    ejabberd_hooks:add(stanza_received, Host,
                       fun stanza_received_hook/4, 80).

stop(Host) ->
    ejabberd_hooks:delete(stanza_received, Host,
                          fun stanza_received_hook/4, 80),
    ejabberd_hooks:delete(stanza_sent, Host,
                          fun stanza_sent_hook/4, 80).

%%%===================================================================
%%% Packet handlers
%%%===================================================================

% "Sent" in the name here referrs to a packet being sent by one of our
% clients to us (the server). Vice-versa for "received".
% This is to maintain consistency with the
% nomenclature in ejabberd_c2s.
-spec stanza_sent_hook(mongoose_acc:t(), jid(),
                       {inet:ip_address(), inet:port_number()},
                       jlib:xmlel()) -> ok.
stanza_sent_hook(Acc, JID, {IP,Port}, Element) ->
    log_packet(JID, IP, Port, Element, false),
    Acc.

-spec stanza_received_hook(mongoose_acc:t(), jid(),
                           {inet:ip_address(), inet:port_number()},
                           jlib:xmlel()) -> ok.
stanza_received_hook(Acc, JID, {IP, Port}, Element) ->
    log_packet(JID, IP, Port, Element, true),
    Acc.


log_packet(JID, IP, Port, Element, Incoming) ->
    {ok, Hostname} = inet:gethostname(),
    ?wocky_traffic_log:put(
       #{user_id => JID#jid.luser,
         resource => JID#jid.resource,
         host => list_to_binary(Hostname),
         ip => ip_port_str(IP, Port),
         packet => exml:to_binary(Element),
         incoming => Incoming}),
    ok.

ip_port_str(IP, Port) -> list_to_binary(
                           inet:ntoa(IP) ++ ":" ++ integer_to_list(Port)).
