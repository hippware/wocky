%%% @copyright 2016+ Hippware, Inc.
%%% @doc Behavior and interface to client notification services
-module(wocky_notification_handler).

-compile({parse_transform, cut}).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include("wocky.hrl").

-export([set_handler/1, enable/3, disable/1, delete/2,
         notify_message/3, notify_bot_event/4]).


%%%===================================================================
%%% Behaviour definition
%%%===================================================================

-callback init() -> ok.

-callback register(User :: binary(),
                   Platform :: binary(),
                   DeviceId :: binary()) ->
    {ok, Endpoint :: binary()} | {error, Error :: any()}.

-callback notify_message(From :: binary(),
                         To :: binary(),
                         Body :: binary()) ->
    ok | {error, Error :: any()}.

-callback notify(To :: binary(),
                 Message :: binary()) ->
    ok | {error, Error :: any()}.


%%%===================================================================
%%% API
%%%===================================================================

-spec set_handler(module()) -> ok.
set_handler(Module) ->
    application:set_env(wocky, notification_handler, Module),
    (handler()):init().

-spec enable(ejabberd:jid(), binary(), binary()) ->
    ok | {error, any()}.
enable(JID, Platform, DeviceId) ->
    User = jid:to_binary(JID),
    ok = lager:debug("Registering device '~s' for user '~s'",
                     [DeviceId, User]),
    case (handler()):register(User, Platform, DeviceId) of
        {ok, Endpoint} ->
            #jid{luser = LUser, lserver = LServer, lresource = LResource} = JID,
            CreatedAt = wocky_db:now_to_timestamp(os:timestamp()),
            ok = wocky_db:insert(LServer, device, #{user => LUser,
                                                    server => LServer,
                                                    resource => LResource,
                                                    platform => Platform,
                                                    device_id => DeviceId,
                                                    endpoint => Endpoint,
                                                    created_at => CreatedAt}),
            ok;

            {error, _} = Error ->
                Error
    end.

-spec disable(ejabberd:jid()) -> ok.
disable(JID) ->
    #jid{luser = LUser, lserver = LServer, lresource = LResource} = JID,
    ok = wocky_db:delete(LServer, device, all, #{user => LUser,
                                                 server => LServer,
                                                 resource => LResource}).

-spec delete(binary(), binary()) -> ok.
delete(LUser, LServer) ->
    wocky_db:delete(LServer, device, all, #{user => LUser, server => LServer}).

-spec notify_message(ejabberd:jid(), ejabberd:jid(), binary()) ->
    ok | {error, any()}.
notify_message(To, From, Body) ->
    do_notify_all(lookup_all_endpoints(To), From, Body).

-spec notify_bot_event(ejabberd:jid(), ejabberd:jid(),
                       binary(), enter | exit) -> ok | {error, any()}.
notify_bot_event(To = #jid{luser = ToUser, lserver = ToServer},
                 #jid{luser = User}, BotTitle, Event) ->
    ok = lager:debug("Sending notification for ~s ~sing bot ~s to ~s",
                     [User, BotTitle, Event, jid:to_binary(To)]),
    Resources = ejabberd_sm:get_user_resources(ToUser, ToServer),
    UserHandle = case ?wocky_user:get_handle(User) of
                     nil -> User;
                     Handle -> Handle
                 end,
    lists:foldl(
      notify_resource_bot_event(To, _, UserHandle, BotTitle, Event, _),
      ok,
      Resources).

notify_resource_bot_event(To, Resource, UserHandle,
                          BotTitle, Event, Result) ->
    FullTo = jid:replace_resource(To, Resource),
    case lookup_endpoint(FullTo) of
        [Endpoint] ->
            Message = case Event of
                          enter ->
                              msg("@~s is near the bot ~s",
                                  [UserHandle, BotTitle]);

                          exit ->
                              msg("@~s is leaving the area for bot ~s",
                                  [UserHandle, BotTitle])
                      end,
            (handler()):notify(Endpoint, Message),
            Result;
        _ ->
            {error, no_endpoint}
    end.


%%%===================================================================
%%% Helpers
%%%===================================================================

handler() ->
    {ok, Handler} = application:get_env(wocky, notification_handler),
    Handler.

lookup_all_endpoints(#jid{luser = LUser, lserver = LServer}) ->
    wocky_db:select_column(LServer, device, endpoint, #{user => LUser,
                                                        server => LServer}).

lookup_endpoint(#jid{luser = LUser, lserver = LServer,
                     lresource = LResource}) ->
    wocky_db:select_column(LServer, device, endpoint, #{user => LUser,
                                                        server => LServer,
                                                        resource => LResource}).

do_notify_all([], _JID, _Message) ->
    ok;
do_notify_all([Endpoint | Rest], JID, Message) ->
    case do_notify_message(Endpoint, JID, Message) of
        {error, _} = E -> E;
        ok -> do_notify_all(Rest, JID, Message)
    end.

do_notify_message(Endpoint, #jid{luser = User, lserver = Server}, Message) ->
    From = wocky_db:select_one(shared, user, handle,
                               #{user => User, server => Server}),
    ok = lager:debug("Sending notification for message from ~s with body '~s'",
                     [From, Message]),
    (handler()):notify_message(Endpoint, From, Message).

msg(Template, Args) ->
    list_to_binary(io_lib:format(Template, Args)).
