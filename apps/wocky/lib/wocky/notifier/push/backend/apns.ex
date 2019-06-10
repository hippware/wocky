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

  def get_response(%Notification{response: response}), do: response

  def get_id(%Notification{id: id}), do: id

  def get_payload(%Notification{payload: payload}), do: payload

  def handle_error(:bad_device_token), do: :invalidate_token

  def handle_error(:unregistered), do: :invalidate_token

  def handle_error(_), do: :retry

  def error_msg(resp), do: Error.msg(resp)

  defp topic, do: get_config(:topic)

  defp add_opts(notification, opts) do
    opts[:background]
    |> if do
      Notification.put_content_available(notification)
    else
      Notification.put_badge(notification, 1)
    end
    |> Notification.put_custom(Keyword.get(opts, :extra_fields, %{}))
  end
end
