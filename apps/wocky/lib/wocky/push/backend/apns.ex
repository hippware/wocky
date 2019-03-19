defmodule Wocky.Push.Backend.APNS do
  @behaviour Wocky.Push.Backend

  alias Pigeon.APNS
  alias Pigeon.APNS.{Error, Notification}
  alias Wocky.Push
  alias Wocky.Push.Event

  require Logger

  def push(params) do
    params.event
    |> build_notification(params.token)
    |> APNS.push(on_response: params.on_response)

    :ok
  end

  def build_notification(event, token) do
    uri = Event.uri(event)

    event
    |> Event.message()
    |> Push.maybe_truncate_message()
    |> Notification.new(token, Push.topic())
    |> Notification.put_badge(1)
    |> Notification.put_custom(%{"uri" => uri})
  end

  def handle_response(
        %Notification{response: resp} = n,
        timeout_pid,
        %Push{user: user, device: device} = params
      ) do
    send(timeout_pid, :push_complete)
    Push.update_metric(resp)
    Push.db_log(log_msg(user, device, n))
    maybe_handle_error(n, %{params | resp: resp})
  end

  defp maybe_handle_error(_n, %Push{resp: :success}), do: :ok

  defp maybe_handle_error(
         n,
         %Push{
           user: user,
           device: device,
           retries: retries,
           resp: resp
         } = params
       ) do
    Logger.error("PN Error: #{Error.msg(resp)}")

    if resp == :bad_device_token do
      Push.invalidate_token(user.id, device, n.device_token)
    else
      Push.do_notify(%{params | retries: retries + 1})
    end
  end

  def error_msg(resp), do: Error.msg(resp)

  defp log_msg(user, device, n) do
    %{
      user_id: user.id,
      device: device,
      token: n.device_token,
      message_id: n.id,
      payload: Push.maybe_extract_payload(n.payload, user),
      response: to_string(n.response),
      details: Error.msg(n.response)
    }
  end
end
