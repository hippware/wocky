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
    #jid{luser = LUser, lserver = LServer, lresource = LResource} = From,
    {ok, State} = handle_request(From, LUser, LServer, LResource, ReqEl),
    make_response(IQ, State);
handle_iq(_From, _To, IQ) ->
    make_error_response(IQ, ?ERRT_NOT_ALLOWED(?MYLANG, <<"not allowed">>)).

handle_request(JID, LUser, LServer, LResource,
               #xmlel{name = <<"enable">>, attrs = Attrs}) ->
    {value, DeviceId} = xml:get_attr(<<"device">>, Attrs),
    {value, Platform} = xml:get_attr(<<"platform">>, Attrs),
    {ok, Endpoint} =
        wocky_notification_handler:register(JID, Platform, DeviceId),
    CreatedAt = wocky_db:now_to_timestamp(os:timestamp()),
    ok = wocky_db:insert(LServer, device, #{user => LUser,
                                            server => LServer,
                                            resource => LResource,
                                            platform => Platform,
                                            device_id => DeviceId,
                                            endpoint => Endpoint,
                                            created_at => CreatedAt}),
    {ok, <<"enabled">>};
handle_request(_, LUser, LServer, LResource,
               #xmlel{name = <<"disable">>}) ->
    ok = wocky_db:delete(LServer, device, all, #{user => LUser,
                                                 server => LServer,
                                                 resource => LResource}),
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
        Body ->
            lists:foreach(notify_message(_, From, Body), lookup_endpoints(To))
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

lookup_endpoints(#jid{luser = LUser, lserver = LServer}) ->
    wocky_db:select_column(LServer, device, endpoint, #{user => LUser,
                                                        server => LServer}).

notify_message(Endpoint, From, Body) ->
    wocky_notification_handler:notify(Endpoint, From, Body).

%% remove_user -------------------------------------------------------

remove_user_hook(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    wocky_db:delete(LServer, device, all, #{user => LUser, server => LServer}).
