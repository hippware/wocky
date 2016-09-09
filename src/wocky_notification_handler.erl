%%% @copyright 2016+ Hippware, Inc.
%%% @doc Behavior and interface to client notification services
-module(wocky_notification_handler).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").

-callback register(User :: binary(), DeviceId :: binary()) ->
    {ok, Endpoint :: binary()}.

-callback notify(From :: binary(), To :: binary(), Message :: binary()) ->
    ok.

-export([register/2, notify/3]).

-spec register(ejabberd:jid(), binary()) -> {ok, binary()}.
register(UserJID, DeviceId) ->
    User = jid:to_binary(UserJID),
    (handler()):register(User, DeviceId).

-spec notify(ejabberd:jid(), ejabberd:jid(), binary()) -> ok.
notify(FromJID, ToJID, Message) ->
    From = jid:to_binary(FromJID),
    To = jid:to_binary(ToJID),
    ok = lager:debug(
           "Sending notification for message from ~s to ~s with body '~s'",
           [From, To, Message]),
    (handler()):notify(From, To, Message).

handler() ->
    {ok, Handler} = application:get_env(wocky, notification_handler),
    Handler.
