defmodule Wocky.Notifier.Push.Backend.APNS do
  @moduledoc """
  Apple Push Notification Service implementation for wocky push system.
  """

  @behaviour Wocky.Notifier.Push.Backend

  use Wocky.Config

  alias Pigeon.APNS
  alias Pigeon.APNS.Error
  alias Pigeon.APNS.Notification
  alias Wocky.Notifier.Push.Event
  alias Wocky.Notifier.Push.Utils

  require Logger

  @impl true
  def push(params) do
    _ =
      params.event
      |> build_notification(params.token)
      |> APNS.push(on_response: params.on_response)

    :ok
  end

  def build_notification(event, token) do
    uri = Event.uri(event)
    opts = Event.opts(event)

    event
    |> Event.message()
    |> Utils.maybe_truncate_message()
    |> Notification.new(token, topic())
    |> Notification.put_custom(%{"uri" => uri})
    |> add_opts(opts)
  end

  @impl true
  def get_response(%Notification{response: response}), do: response

  @impl true
  def get_id(%Notification{id: id}), do: id

  @impl true
  def get_payload(%Notification{payload: payload}), do: payload

  @impl true
  def handle_error(:bad_device_token), do: :invalidate_token

  def handle_error(:unregistered), do: :invalidate_token

  def handle_error(_), do: :retry

  @impl true
  def error_msg(resp), do: Error.msg(resp)

  defp topic, do: get_config(:topic)

  defp add_opts(notification, opts) do
    notification
    |> add_badge_or_content_avail(Keyword.get(opts, :background, false))
    |> maybe_put(&Notification.put_sound/2, Keyword.get(opts, :sound))
    |> Notification.put_custom(Keyword.get(opts, :extra_fields, %{}))
  end

  defp add_badge_or_content_avail(notification, false),
    do: Notification.put_badge(notification, 1)

  defp add_badge_or_content_avail(notification, true),
    do: Notification.put_content_available(notification)

  defp maybe_put(notification, _fun, nil),
    do: notification

  defp maybe_put(notification, fun, val),
    do: fun.(notification, val)
end
