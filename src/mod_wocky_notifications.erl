%%% @copyright 2016+ Hippware, Inc.
%%% @doc Module to handle message push notifications
-module(mod_wocky_notifications).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include("wocky.hrl").

%% gen_mod behaviour
-behaviour(gen_mod).
-export([start/2, stop/1]).

%% Hook callbacks
-export([user_receive_packet_hook/4, offline_message_hook/3]).


%%%===================================================================
%%% gen_mod implementation
%%%===================================================================

start(Host, _Opts) ->
    wocky_util:add_hooks(hooks(), Host, ?MODULE, 30).

stop(Host) ->
    wocky_util:delete_hooks(hooks(), Host, ?MODULE, 30).

hooks() ->
    [{user_receive_packet, user_receive_packet_hook},
     {offline_message_hook, offline_message_hook}].


%%%===================================================================
%%% Hook callbacks
%%%===================================================================

%% offline_message_hook ----------------------------------------------
offline_message_hook(From, To, Packet) ->
    handle_incoming_message(From, To, Packet).

%% user_receive_packet -----------------------------------------------
user_receive_packet_hook(_JID, From, To, Packet) ->
    handle_incoming_message(From, To, Packet).

handle_incoming_message(From, To, Packet) ->
    case should_notify(Packet) of
        false -> ok;
        Body -> notify_message(From, To, Body)
    end.

should_notify(Packet) ->
    is_chat(Packet) andalso get_body(Packet).

is_chat(#xmlel{name = <<"message">>, attrs = Attrs}) ->
    case xml:get_attr_s(<<"type">>, Attrs) of
        <<"chat">> -> true;
        _ -> false
    end;
is_chat(_Packet) -> false.

get_body(Packet) ->
    case xml:get_subtag(Packet, <<"body">>) of
        false -> false;
        BodyTag -> xml:get_tag_cdata(BodyTag)
    end.

notify_message(From, To, Body) ->
    wocky_notification_handler:notify(From, To, Body).
