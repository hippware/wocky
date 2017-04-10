%%% @copyright 2016+ Hippware, Inc.
%%%
%%% @doc Module implementing multicast addressing
%%% See https://github.com/hippware/tr-wiki/wiki/Multicast-Addressing
%%%
-module(mod_wocky_multicast).

-behaviour(gen_mod).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).

%% gen_mod handlers
-export([start/2, stop/1]).

-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/ejabberd.hrl").
-include("wocky_roster.hrl").

%%%===================================================================
%%% gen_mod handlers
%%%===================================================================

start(Host, _Opts) ->
    mod_disco:register_feature(Host, ?NS_ADDRESS),
    ejabberd_hooks:add(filter_local_packet, Host,
                       fun filter_local_packet_hook/1, 80).

stop(Host) ->
    mod_disco:unregister_feature(Host, ?NS_ADDRESS),
    ejabberd_hooks:delete(filter_local_packet, Host,
                          fun filter_local_packet_hook/1, 80).

%%%===================================================================
%%% Incoming packet handler
%%%===================================================================

-type filter_packet() :: {ejabberd:jid(), ejabberd:jid(), jlib:xmlel()}.
-spec filter_local_packet_hook(filter_packet() | drop) ->
    filter_packet() | drop.
filter_local_packet_hook(P = {_, _, Packet = #xmlel{name = Name}})
      when Name =:= <<"message">> orelse
           Name =:= <<"presence">> ->
    case xml:get_subtag(Packet, <<"addresses">>) of
        false -> P;
        AddressesEl -> multicast_packet(P, AddressesEl)
    end;
filter_local_packet_hook(Other) ->
    Other.

multicast_packet(P = {From, To, Packet}, AddressesEl) ->
    case multicast_packet(From, To, Packet, AddressesEl) of
        ok -> drop;
        {error, _} -> P
    end.

multicast_packet(From, To, Packet, AddressesEl) ->
    do([error_m ||
        check_ns(AddressesEl),
        check_to(From, To),
        Addresses <- get_addresses(From, AddressesEl),
        CleanPacket <- strip_addresses(Packet, AddressesEl),
        forward_message(From, Addresses, CleanPacket)
       ]).

check_ns(AddressesEl) ->
    case xml:get_tag_attr(<<"xmlns">>, AddressesEl) of
        {value, ?NS_ADDRESS} -> ok;
        _ -> {error, not_address_ns}
    end.

check_to(#jid{lserver = LServer},
         #jid{luser = <<>>, lserver = LServer, lresource = <<>>}) ->
    case wocky_xmpp_app:server() of
        LServer -> ok;
        _ -> {error, not_local_server}
    end;
check_to(_, _) ->
    {error, not_users_server}.

get_addresses(From, AddressesEl) ->
    {ok, lists:usort(
           lists:foldl(
             get_address(From, _, _), [], AddressesEl#xmlel.children))}.

get_address(From, #xmlel{name = <<"address">>, attrs = Attrs}, Acc) ->
    case xml:get_attr(<<"type">>, Attrs) of
        {value, <<"friends">>} ->
            get_contact_addresses(
              wocky_db_roster:is_friend(_), From) ++ Acc;
        {value, <<"followers">>} ->
            get_contact_addresses(
              wocky_db_roster:is_follower(_), From) ++ Acc;
        {value, <<"to">>} ->
            add_target(Attrs, Acc);
        {value, Type} ->
            ok = lager:warning("Unsupported address type received: ~p",
                               [Type]),
            Acc;
        false ->
            ok = lager:warning("Missing type attribute on address element"),
            Acc
    end.

get_contact_addresses(FilterFun, #jid{luser = LUser, lserver = LServer}) ->
    Roster = wocky_db_roster:get_roster(LUser, LServer),
    Filtered = lists:filter(FilterFun, Roster),
    [jid:make(CJ) || #wocky_roster{contact_jid = CJ} <- Filtered].

add_target(Attrs, Acc) ->
    case xml:get_attr(<<"jid">>, Attrs) of
        {value, JID} ->
            maybe_add_jid(jid:from_binary(JID), Acc);
        false ->
            ok = lager:warning("Missing 'jid' attribute on 'to' element"),
            Acc
    end.

maybe_add_jid(error, Acc) -> Acc;
maybe_add_jid(JID = #jid{}, Acc) -> [JID | Acc].

strip_addresses(Packet = #xmlel{children = Children}, AddressesEl) ->
    {ok, Packet#xmlel{children = lists:delete(AddressesEl, Children)}}.

forward_message(From, Tos, Packet) ->
    lists:foreach(ejabberd_router:route(From, _, Packet), Tos).
