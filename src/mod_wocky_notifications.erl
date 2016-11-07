%%% @copyright 2016+ Hippware, Inc.
%%% @doc Module to handle message push notifications
-module(mod_wocky_notifications).

-compile({parse_transform, cut}).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include("wocky.hrl").

%% gen_mod behaviour
-behaviour(gen_mod).
-export([start/2, stop/1]).

%% Hook callbacks
-export([user_receive_packet_hook/4,
         offline_message_hook/3,
         remove_user_hook/2]).

%% IQ handler
-export([handle_iq/3]).

-ignore_xref([user_receive_packet_hook/4,
              offline_message_hook/3,
              remove_user_hook/2,
              handle_iq/3]).


%%%===================================================================
%%% gen_mod implementation
%%%===================================================================

-spec start(ejabberd:server(), list()) -> any().
start(Host, _Opts) ->
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_NOTIFICATIONS,
                                  ?MODULE, handle_iq, parallel),
    wocky_util:add_hooks(hooks(), Host, ?MODULE, 30).

-spec stop(ejabberd:server()) -> ok.
stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_NOTIFICATIONS),
    wocky_util:delete_hooks(hooks(), Host, ?MODULE, 30).

hooks() ->
    [{user_receive_packet,  user_receive_packet_hook},
     {offline_message_hook, offline_message_hook},
     {remove_user,          remove_user_hook}].


%%%===================================================================
%%% IQ handler
%%%===================================================================

-spec handle_iq(ejabberd:jid(), ejabberd:jid(), iq()) -> iq().
handle_iq(From, _To, IQ = #iq{type = set, sub_el = ReqEl}) ->
    case handle_request(From, ReqEl) of
        {ok, State} ->
            make_response(IQ, State);

        {error, _} ->
            Error =
                ?ERRT_SERVICE_UNAVAILABLE(?MYLANG,
                                          <<"service unavailable">>),
            make_error_response(IQ, Error)
    end;
handle_iq(_From, _To, IQ) ->
    make_error_response(IQ, ?ERRT_NOT_ALLOWED(?MYLANG, <<"not allowed">>)).

handle_request(JID, #xmlel{name = <<"enable">>, attrs = Attrs}) ->
    {value, DeviceId} = xml:get_attr(<<"device">>, Attrs),
    {value, Platform} = xml:get_attr(<<"platform">>, Attrs),
    case wocky_notification_handler:enable(JID, Platform, DeviceId) of
        ok -> {ok, <<"enabled">>};
        {error, _} = Error -> Error
    end;
handle_request(JID, #xmlel{name = <<"disable">>}) ->
    ok = wocky_notification_handler:disable(JID),
    {ok, <<"disabled">>}.

make_error_response(IQ, ErrStanza) ->
    ok = lager:warning("Error on notification IQ request: ~p", [ErrStanza]),
    IQ#iq{type = error, sub_el = ErrStanza}.

make_response(IQ, State) ->
    IQ#iq{type = result,
          sub_el = #xmlel{name = State,
                          attrs = [{<<"xmlns">>, ?NS_NOTIFICATIONS}]}}.


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
        Body -> wocky_notification_handler:notify_message(To, From, Body)
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

%% remove_user -------------------------------------------------------
remove_user_hook(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    wocky_notification_handler:delete(LUser, LServer).
