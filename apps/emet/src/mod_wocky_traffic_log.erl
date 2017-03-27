%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing traffic logging
%%%
-module(mod_wocky_traffic_log).

-behaviour(gen_mod).

%% gen_mod handlers
-export([start/2, stop/1]).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include("wocky_roster.hrl").

-define(DEFAULT_EXPIRE, 3600).

%%%===================================================================
%%% gen_mod handlers
%%%===================================================================

start(Host, Opts) ->
    wocky_util:set_config_from_opt(expire, traffic_log_expire,
                                   ?DEFAULT_EXPIRE, Opts),
    ejabberd_hooks:add(stanza_sent, Host,
                       fun stanza_sent_hook/3, 80),
    ejabberd_hooks:add(stanza_received, Host,
                       fun stanza_received_hook/3, 80).

stop(Host) ->
    ejabberd_hooks:delete(stanza_received, Host,
                          fun stanza_received_hook/3, 80),
    ejabberd_hooks:delete(stanza_sent, Host,
                          fun stanza_sent_hook/3, 80).

%%%===================================================================
%%% Packet handlers
%%%===================================================================

% "Sent" in the name here referrs to a packet being sent by one of our
% clients to us (the server). Vice-versa for "received".
% This is to maintain consistency with the
% nomenclature in ejabberd_c2s.
-spec stanza_sent_hook(jid(), {inet:ip_address(), inet:port_number()},
                       jlib:xmlel()) -> ok.
stanza_sent_hook(JID, {IP,Port}, Element) ->
    log_packet(JID, IP, Port, Element, false).

-spec stanza_received_hook(jid(), {inet:ip_address(), inet:port_number()},
                           jlib:xmlel()) -> ok.
stanza_received_hook(JID, {IP, Port}, Element) ->
    log_packet(JID, IP, Port, Element, true).


log_packet(JID, IP, Port, Element, Incoming) ->
    {ok, Hostname} = inet:gethostname(),
    Query = "INSERT INTO traffic_log "
            "(user, resource, timestamp, ip, server, packet, incoming) "
            "VALUES (?, ?, ?, ?, ?, ?, ?) USING TTL ?",
    Values = #{user => jid:to_binary(jid:to_bare(JID)),
               resource => JID#jid.lresource,
               timestamp => now,
               ip => ip_port_str(IP, Port),
               server => Hostname,
               packet => exml:to_binary(Element),
               incoming => Incoming,
               '[ttl]' => expiry()},
    % Use 'one' here for speed - we shouldn't get any clashes and if we do
    % it's not the end of the world.
    {ok, _} = wocky_db:query(shared, Query, Values, one),
    ok.

ip_port_str(IP, Port) -> inet:ntoa(IP) ++ ":" ++ integer_to_list(Port).

expiry() -> ejabberd_config:get_local_option(traffic_log_expire).
