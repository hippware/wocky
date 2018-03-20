%%% @copyright 2016+ Hippware, Inc.
%%% @doc Module to handle message push notifications
-module(mod_wocky_notifications).

-compile({parse_transform, cut}).

-include("wocky.hrl").
-include("wocky_roster.hrl").

%% gen_mod behaviour
-behaviour(gen_mod).
-export([start/2, stop/1]).

%% Hook callbacks
-export([user_send_packet_hook/4,
         roster_updated_hook/5,
         remove_user_hook/3]).

%% IQ handler
-export([handle_iq/3]).


%%%===================================================================
%%% gen_mod implementation
%%%===================================================================

-spec start(ejabberd:server(), list()) -> any().
start(Host, _Opts) ->
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_NOTIFICATIONS,
                                  ?MODULE, handle_iq, parallel),
    wocky_util:add_hooks(hooks(), Host, ?MODULE, 100).

-spec stop(ejabberd:server()) -> ok.
stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_NOTIFICATIONS),
    wocky_util:delete_hooks(hooks(), Host, ?MODULE, 100).

hooks() ->
    [{user_send_packet, user_send_packet_hook},
     {roster_updated,   roster_updated_hook},
     {remove_user,      remove_user_hook}].


%%%===================================================================
%%% IQ handler
%%%===================================================================

-spec handle_iq(ejabberd:jid(), ejabberd:jid(), iq()) -> iq().
handle_iq(From, _To, IQ = #iq{type = set, sub_el = ReqEl}) ->
    {ok, State} = handle_request(From, ReqEl),
    make_response(IQ, State);
handle_iq(_From, _To, IQ) ->
    make_error_response(IQ, ?ERRT_NOT_ALLOWED(?MYLANG, <<"not allowed">>)).

handle_request(J, #xmlel{name = <<"enable">>, attrs = Attrs}) ->
    {value, DeviceId} = xml:get_attr(<<"device">>, Attrs),
    ok = ?wocky_push:enable(J#jid.luser, J#jid.lresource, DeviceId),
    {ok, <<"enabled">>};
handle_request(J, #xmlel{name = <<"disable">>}) ->
    ok = ?wocky_push:disable(J#jid.luser, J#jid.lresource),
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

%% user_send_packet --------------------------------------------------
user_send_packet_hook(Acc, From, To, Packet) ->
    case notify_type(From, To, Packet) of
        chat ->
            Body = get_body(Packet),
            Image = get_image(Packet),
            Sender = ?wocky_user:get_by_jid(From),
            Recipient = ?wocky_user:get_by_jid(To),
            ConversationID =
                ?wocky_conversation:get_id(To#jid.luser,
                                           jid:to_binary(jid:to_bare(From))),
            Event = ?new_message_event:new(
                       #{to => Recipient,
                         from => Sender,
                         body => Body,
                         image => Image,
                         conversation_id => ConversationID}),
            Result = ?wocky_push:notify_all(To#jid.luser, Event),
            mongoose_acc:put(result, Result, Acc);

        geofence_share ->
            ID = get_bot_id(Packet),
            Bot = ?wocky_bot:get(ID),
            FromUser = ?wocky_user:get_by_jid(From),
            ToUser = ?wocky_user:get_by_jid(To),
            Result = wocky_bot_users:send_geofence_share_notification(
              FromUser, ToUser, Bot),
            mongoose_acc:put(result, Result, Acc);

        _Else ->
            mongoose_acc:put(result, ok, Acc)
    end.

notify_type(_From, To, Packet) ->
    IsChat =
        has_destination(To) andalso
        is_chat_message(Packet) andalso
        (has_body(Packet) orelse has_image(Packet)),

    IsGeofenceShare =
        has_destination(To) andalso
        is_headline_message(Packet) andalso
        has_geofence_share(Packet) andalso
        has_valid_bot_id(Packet),

    case {IsChat, IsGeofenceShare} of
        {true, _} -> chat;
        {_, true} -> geofence_share;
        _ -> undefined
    end.


has_destination(#jid{luser = <<>>}) -> false;
has_destination(_) -> true.

is_chat_message(#xmlel{name = <<"message">>, attrs = Attrs}) ->
    xml:get_attr_s(<<"type">>, Attrs) =:= <<"chat">>;
is_chat_message(_) ->
    false.

is_headline_message(#xmlel{name = <<"message">>, attrs = Attrs}) ->
    xml:get_attr_s(<<"type">>, Attrs) =:= <<"headline">>;
is_headline_message(_) ->
    false.

has_geofence_share(Packet) ->
    exml_query:path(Packet, [{element, <<"bot">>},
                             {element, <<"action">>},
                             cdata])
    =:= <<"geofence share">>.

has_valid_bot_id(Packet) ->
    V = get_bot_id(Packet),
    V =/= undefined andalso ?wocky_id:'valid?'(V).

has_body(Packet) ->
    get_body(Packet) =/= <<"">>.

has_image(Packet) ->
    get_image(Packet) =/= <<"">>.

get_body(Packet) ->
    exml_query:path(Packet, [{element, <<"body">>}, cdata], <<"">>).

get_image(Packet) ->
    exml_query:path(Packet,
                    [{element, <<"image">>},
                     {element, <<"url">>},
                     cdata],
                    <<"">>).

get_bot_id(Packet) ->
    exml_query:path(Packet, [{element, <<"bot">>},
                             {element, <<"id">>},
                             cdata]).

%% roster_updated ----------------------------------------------------
roster_updated_hook(
  Acc,
  UserID,
  _Server,
  #wocky_roster{subscription = OldSubscription},
  #wocky_roster{subscription = NewSubscription, contact_jid = ContactJID}) ->
    case {OldSubscription, NewSubscription} of
        {none, from} ->
            User = ?wocky_repo:get(?wocky_user, UserID),
            Follower = ?wocky_user:get_by_jid(jid:make(ContactJID)),
            Event = ?new_follower_event:new(#{user => User,
                                              follower => Follower}),
            Result = ?wocky_push:notify_all(UserID, Event),
            mongoose_acc:put(result, Result, Acc);
        _ ->
            mongoose_acc:put(result, ok, Acc)
    end.

%% remove_user -------------------------------------------------------
remove_user_hook(Acc, User, _Server) ->
    ?wocky_push:purge(User),
    Acc.
