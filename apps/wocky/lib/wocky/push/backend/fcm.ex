defmodule Wocky.Push.Backend.FCM do
  @moduledoc """
  Firebase Cloud Messaging (Android) implementation for wocky push system
  """

  @behaviour Wocky.Push.Backend

  alias Pigeon.FCM
  alias Pigeon.FCM.Notification
  alias Wocky.Push
  alias Wocky.Push.Event

  def push(params) do
    params.event
    |> build_notification(params.token)
    |> FCM.push(on_response: params.on_response, timeout: Push.timeout())

    :ok
  end

  defp build_notification(event, token) do
    event_msg =
      event
      |> Event.message()
      |> Push.maybe_truncate_message()

    token
    |> Notification.new(
      %{"title" => "tinyrobot", "body" => event_msg},
      %{"url" => Event.uri(event)}
    )
    |> Notification.put_restricted_package_name(package())
  end

  def get_response(%Notification{status: :success, response: response}),
    do: get_response(response)

  def get_response(%Notification{status: status}), do: status

  def get_response([{:update, _regids}]), do: :success
  def get_response([{response, _regid}]), do: response

  def get_id(%Notification{message_id: id}), do: id

  def get_payload(%Notification{payload: payload}), do: payload

  def handle_error(:not_registered), do: :invalidate_token
  def handle_error(:invalid_registration), do: :invalidate_token
  def handle_error(:missing_registration), do: :invalidate_token
  def handle_error(_), do: :retry

  def error_msg(resp), do: to_string(resp.status)

  defp package, do: Confex.get_env(:wocky, __MODULE__)[:package]
end
