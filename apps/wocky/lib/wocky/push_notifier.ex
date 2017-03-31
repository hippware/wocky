defmodule Wocky.PushNotifier do
  @moduledoc "Behavior and interface to client push notification services"

  alias Wocky.Device
  alias Wocky.User

  require Logger

  @type endpoint :: binary

  # ===================================================================
  # Behaviour definition

  @callback init :: :ok

  @callback register(user :: User.id,
                     resource :: User.resource,
                     platform :: Device.platform,
                     device :: Device.device) ::
    {:ok, endpoint :: endpoint} | {:error, reason :: any}

  @callback push(body :: binary, to :: endpoint) ::
    :ok | {:error, reason :: any}

  # ===================================================================
  # API

  @spec enable(User.id, User.resource, Device.platform, Device.device) ::
    :ok | {:error, any}
  def enable(user_id, resource, platform, device) do
    :ok = Logger.debug("Registering device '#{device}' for '#{user_id}'")
    case (handler()).register(user_id, resource, platform, device) do
      {:ok, endpoint} ->
        Device.update(user_id, resource, platform, device, endpoint)

      {:error, _} = error ->
        error
    end
  end

  @spec disable(User.id, User.resource) :: :ok
  def disable(user_id, resource) do
    Device.delete(user_id, resource)
  end

  @spec delete(User.id) :: :ok
  def delete(user_id) do
    Device.delete_all(user_id)
  end

  def push(user_id, resource, from, message) do
    user_id
    |> Device.get_endpoint(resource)
    |> do_push(from, message)
  end

  def push_all(user_id, from, message) do
    for endpoint <- Device.get_all_endpoints(user_id) do
      do_push(endpoint, from, message)
    end
  end

  defp do_push(nil, _from, _message), do: :ok
  defp do_push(endpoint, from, message) do
    message
    |> format_message(from)
    |> (handler()).push(endpoint)
  end

  defp format_message(body, nil), do: body
  defp format_message(body, from), do: "From #{from}:\n#{body}"

  # -spec notify_message(ejabberd:jid(), ejabberd:jid(), binary()) ->
  #     ok | {error, any()}.
  # notify_message(To, From, Body) ->
  #     do_notify_all(lookup_all_endpoints(To), From, Body).

  # -spec notify_bot_event(ejabberd:jid(), ejabberd:jid(),
  #                        binary(), enter | exit) -> ok | {error, any()}.
  # notify_bot_event(To = #jid{luser = ToUser, lserver = ToServer},
  #                  #jid{luser = User, lserver = Server},
  #                  BotTitle, Event) ->
  #     ok = lager:debug("Sending notification for ~s ~sing bot ~s to ~s",
  #                      [User, BotTitle, Event, jid:to_binary(To)]),
  #     Resources = ejabberd_sm:get_user_resources(ToUser, ToServer),
  #     UserHandle = case ?wocky_user:find(Server, User) of
  #                      #{handle := nil} -> User;
  #                      #{handle := Handle} -> Handle;
  #                      nil -> User
  #                  end,
  #     lists:foldl(
  #       notify_resource_bot_event(To, _, UserHandle, BotTitle, Event, _),
  #       ok,
  #       Resources).

  # notify_resource_bot_event(To, Resource, UserHandle,
  #                           BotTitle, Event, Result) ->
  #     FullTo = jid:replace_resource(To, Resource),
  #     case lookup_endpoint(FullTo) of
  #         [Endpoint] ->
  #             Message = case Event of
  #                           enter ->
  #                               msg("@~s is near the bot ~s",
  #                                   [UserHandle, BotTitle]);

  #                           exit ->
  #                               msg("@~s is leaving the area for bot ~s",
  #                                   [UserHandle, BotTitle])
  #                       end,
  #             (handler()):notify(Endpoint, Message),
  #             Result;
  #         _ ->
  #             {error, no_endpoint}
  #     end.

  # ===================================================================
  # Helpers

  defp handler, do: Application.fetch_env!(:wocky, :notification_handler)

  # do_notify_all([], _JID, _Message) ->
  #     ok;
  # do_notify_all([Endpoint | Rest], JID, Message) ->
  #     case do_notify_message(Endpoint, JID, Message) of
  #         {error, _} = E -> E;
  #         ok -> do_notify_all(Rest, JID, Message)
  #     end.

  # do_notify_message(Endpoint, #jid{user = User, server = Server}, Message) ->
  #     From = wocky_db:select_one(shared, user, handle,
  #                                #{user => User, server => Server}),
  #     ok = lager:debug(
  #     "Sending notification for message from ~s with body '~s'",
  #                      [From, Message]),
  #     (handler()):notify_message(Endpoint, From, Message).

  # msg(Template, Args) ->
  #     list_to_binary(io_lib:format(Template, Args)).
end
