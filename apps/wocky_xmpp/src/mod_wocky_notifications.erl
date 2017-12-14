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
-export([user_send_packet_hook/3,
         roster_process_item_hook/2,
         remove_user_hook/2]).

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
    [{user_send_packet,     user_send_packet_hook},
     {roster_process_item,  roster_process_item_hook},
     {remove_user,          remove_user_hook}].


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
user_send_packet_hook(From, To, Packet) ->
    case should_notify(From, To, Packet) of
        true ->
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
            ?wocky_push:notify_all(To#jid.luser, Event);

        _Else ->
            ok
    end.

should_notify(_From, To, Packet) ->
    has_destination(To) andalso
    is_chat_message(Packet) andalso
    (has_body(Packet) orelse has_image(Packet)).

has_destination(#jid{luser = <<>>}) -> false;
has_destination(_) -> true.

is_chat_message(#xmlel{name = <<"message">>, attrs = Attrs}) ->
    xml:get_attr_s(<<"type">>, Attrs) =:= <<"chat">>;
is_chat_message(_) ->
    false.

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


%% roster_process_item -----------------------------------------------
roster_process_item_hook(Item, _Host) ->
    case Item#wocky_roster.subscription of
        remove ->
            ok;

        _ ->
            UserID = Item#wocky_roster.user,
            User = ?wocky_repo:get(?wocky_user, UserID),
            Follower = ?wocky_user:get_by_jid(Item#wocky_roster.contact_jid),
            Event = ?new_follower_event:new(#{user => User,
                                              follower => Follower}),
            ?wocky_push:notify_all(UserID, Event)
    end,
    Item.

%% remove_user -------------------------------------------------------
remove_user_hook(User, _Server) ->
    ?wocky_push:purge(User).
