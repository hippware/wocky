defmodule Wocky.PushNotifier do
  @moduledoc "Behavior and interface to client push notification services"

  use Wocky.JID

  alias Wocky.Device

  require Logger

  @type endpoint :: binary

  @message_limit 512

  # ===================================================================
  # API

  @spec init :: :ok
  def init do
    :ok
  end

  @spec enable(JID.t, Device.platform, Device.token) :: :ok | {:error, any}
  def enable(jid(luser: user, lresource: resource), platform, token) do
    Device.update(user, resource, platform, token)
  end

  @spec disable(JID.t) :: :ok
  def disable(jid(luser: user, lresource: resource)) do
    Device.delete(user, resource)
  end

  @spec delete(JID.t) :: :ok
  def delete(jid(luser: user)) do
    Device.delete_all(user)
  end

  @spec push(JID.t, binary) :: :ok
  def push(jid(luser: user, lresource: resource), message) do
    user
    |> Device.get_token(resource)
    |> do_push(message)
  end

  @spec push_all(JID.t, binary) :: :ok
  def push_all(jid(luser: user), message) do
    for token <- Device.get_all_tokens(user) do
      do_push(token, message)
    end
    :ok
  end

  # ===================================================================
  # Helpers

  defp do_push(nil, _body), do: :ok
  defp do_push(token, body) do
    body
    |> maybe_truncate_message
    |> make_payload
    |> maybe_push(token, enabled?())
  end

  defp maybe_truncate_message(message) do
    if byte_size(message) > @message_limit do
      String.slice(message, 0, @message_limit - 3) <> "..."
    else
      message
    end
  end

  defp make_payload(message) do
    %{alert: message, badge: 1}
  end

  defp maybe_push(_body, _token, false), do: :ok
  defp maybe_push(body, token, true) do
    Pushex.push(body, to: token, using: :apns)
    :ok
  end

  defp enabled? do
    Confex.get(:wocky, :enable_push_notifications)
  end
end
