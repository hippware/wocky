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

-spec notify(binary(), ejabberd:jid(), binary()) -> ok.
notify(Endpoint, FromJID, Message) ->
    From = jid:to_binary(FromJID),
    ok = lager:debug(
           "Sending notification for message from ~s with body '~s'",
           [From, Message]),
    (handler()):notify(Endpoint, From, Message).

handler() ->
    {ok, Handler} = application:get_env(wocky, notification_handler),
    Handler.
