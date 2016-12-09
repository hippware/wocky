%%% @copyright 2016+ Hippware, Inc.
%%% @doc Behavior and interface to client notification services
-module(wocky_notification_handler).

-compile({parse_transform, cut}).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").

-export([enable/3, disable/1, delete/2,
         notify_message/3]).


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
                                                    device_id => DeviceId,
                                                    resource => LResource,
                                                    platform => Platform,
                                                    endpoint => Endpoint,
                                                    created_at => CreatedAt}),
            ok;

            {error, _} = Error ->
                Error
    end.

-spec disable(ejabberd:jid()) -> ok.
disable(#jid{luser = LUser, lserver = LServer} = JID) ->
    case lookup_resource(JID, device_id) of
        [DeviceId] ->
            ok = wocky_db:delete(LServer, device, all,
                                 #{user => LUser,
                                   server => LServer,
                                   device_id => DeviceId});

        _Else ->
            ok
    end.

-spec delete(binary(), binary()) -> ok.
delete(LUser, LServer) ->
    wocky_db:delete(LServer, device, all, #{user => LUser, server => LServer}).

-spec notify_message(ejabberd:jid(), ejabberd:jid(), binary()) ->
    ok | {error, any()}.
notify_message(To, From, Body) ->
    do_notify_all(lookup_all_endpoints(To), From, Body).

%%%===================================================================
%%% Helpers
%%%===================================================================

handler() ->
    {ok, Handler} = application:get_env(wocky, notification_handler),
    Handler.

lookup_all_endpoints(#jid{luser = LUser, lserver = LServer}) ->
    wocky_db:select_column(LServer, device, endpoint, #{user => LUser,
                                                        server => LServer}).

lookup_resource(#jid{luser = LUser, lserver = LServer,
                     lresource = LResource}, Field) ->
    wocky_db:select_column(LServer, device_resource, Field,
                           #{user => LUser,
                             server => LServer,
                             resource => LResource}).

do_notify_all([], _JID, _Message) ->
    ok;
do_notify_all([Endpoint | Rest], #jid{luser = User} = JID, Message) ->
    %% TODO: This is an ugly kludge to handle the problem of users receiving
    %% multiple push notifications for a single event. The code will stop
    %% sending notifications once one is delivered, but this isn't really what
    %% we want for the final product. It works for now since we expect users
    %% to have a single device in the beta.
    case do_notify_message(Endpoint, JID, Message) of
        ok -> ok;
        {error, Reason} ->
            ok = lager:debug("Failed to send push notification to ~s: ~p",
                             [User, Reason]),
            case Rest of
                [] -> {error, Reason};
                _ -> do_notify_all(Rest, JID, Message)
            end
    end.

do_notify_message(Endpoint, #jid{user = User, server = Server}, Message) ->
    From = wocky_db:select_one(shared, user, handle,
                               #{user => User, server => Server}),
    ok = lager:debug("Sending notification for message from ~s with body '~s'",
                     [From, Message]),
    (handler()):notify_message(Endpoint, From, Message).
