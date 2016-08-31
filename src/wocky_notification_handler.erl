%%% @copyright 2016+ Hippware, Inc.
%%% @doc Behavior and interface to client notification services
-module(wocky_notification_handler).

-callback notify(From :: binary(), To :: binary(), Message :: binary()) -> ok.

-export([notify/3]).

notify(FromJID, ToJID, Message) ->
    From = jid:to_binary(FromJID),
    To = jid:to_binary(ToJID),
    ok = lager:debug(
           "Sending notification for message from ~s to ~s with body '~s'",
           [From, To, Message]),
    {ok, Handler} = application:get_env(wocky, notification_handler),
    Handler:notify(From, To, Message).
