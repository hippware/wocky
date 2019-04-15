defmodule Wocky.Notifier.Push.Backend.APNS do
  @moduledoc """
  Apple Push Notification Service implementation for wocky push system.
  """

  @behaviour Wocky.Notifier.Push.Backend

  alias Pigeon.APNS
  alias Pigeon.APNS.{Error, Notification}
  alias Wocky.Notifier.Push.{Event, Utils}

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

    event
    |> Event.message()
    |> Utils.maybe_truncate_message()
    |> Notification.new(token, topic())
    |> Notification.put_badge(1)
    |> Notification.put_custom(%{"uri" => uri})
  end

  def get_response(%Notification{response: response}), do: response

  def get_id(%Notification{id: id}), do: id

  def get_payload(%Notification{payload: payload}), do: payload

  def handle_error(:bad_device_token), do: :invalidate_token

  def handle_error(_), do: :retry

  def error_msg(resp), do: Error.msg(resp)

  defp topic, do: Confex.get_env(:wocky, __MODULE__)[:topic]
end
