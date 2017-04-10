%%% @copyright 2016+ Hippware, Inc.
%%% @doc Behavior and interface to client notification services
-module(wocky_notification_handler).

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
