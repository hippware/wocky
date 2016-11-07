%%% @copyright 2016+ Hippware, Inc.
%%% @doc Behavior and interface to client notification services
-module(wocky_notification_handler).

-compile({parse_transform, cut}).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").

-export([enable/3, disable/1, delete/2,
         notify_message/3, notify_bot_event/3]).


%%%===================================================================
%%% Behaviour definition
%%%===================================================================

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

-spec notify_bot_event(ejabberd:jid(), binary(), binary()) ->
    ok | {error, any()}.
notify_bot_event(To, Bot, Event) ->
    ok = lager:debug("Sending notification for ~s ~sing bot ~s",
                     [jid:to_binary(To), Bot, Event]),
    case lookup_endpoint(To) of
        [Endpoint] ->
            Message = case Event of
                          enter ->
                              msg("You are near the bot ~s", [Bot]);

                          exit ->
                              msg("You are leaving the area for bot ~s", [Bot])
                      end,
            (handler()):notify(Endpoint, Message);

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

do_notify_message(Endpoint, #jid{user = User, server = Server}, Message) ->
    From = wocky_db:select_one(shared, user, handle,
                               #{user => User, server => Server}),
    ok = lager:debug("Sending notification for message from ~s with body '~s'",
                     [From, Message]),
    (handler()):notify_message(Endpoint, From, Message).

msg(Template, Args) ->
    list_to_binary(io_lib:format(Template, Args)).
