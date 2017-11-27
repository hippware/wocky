defmodule Wocky.Push do
  @moduledoc """
  The Push context. Single interface for push notifications.
  """

  import Ecto.Query, warn: false

  alias Pigeon.APNS
  alias Pigeon.APNS.Error
  alias Pigeon.APNS.Notification
  alias Wocky.Repo
  alias Wocky.Push.Event
  alias Wocky.Push.Log
  alias Wocky.Push.Sandbox
  alias Wocky.Push.Token

  require Logger

  @type message :: binary

  @message_limit 512

  # ===================================================================
  # Push Token API

  @spec enable(Wocky.User.id, Wocky.User.resource, Token.token)
    :: :ok
  def enable(user_id, resource, token) do
    %{user_id: user_id,
      resource: resource,
      token: token}
    |> Token.register_changeset
    |> Repo.insert!(
      on_conflict: [set: [valid: true, enabled_at: DateTime.utc_now]],
      conflict_target: [:user_id, :resource, :token]
    )

    :ok
  end

  @spec disable(Wocky.User.id, Wocky.User.resource) :: :ok
  def disable(user_id, resource) do
    Repo.update_all(
      from(Token, where: [user_id: ^user_id, resource: ^resource, valid: true]),
      set: [valid: false, disabled_at: DateTime.utc_now]
    )

    :ok
  end

  @spec purge(Wocky.User.id) :: :ok
  def purge(user_id) do
    Repo.delete_all(from Token, where: [user_id: ^user_id])

    :ok
  end

  # ===================================================================
  # Push Notification API

  @spec notify(Wocky.User.id, Wocky.User.resource, any) :: :ok
  def notify(user_id, resource, event) do
    if enabled?() do
      message = Event.format(event)

      user_id
      |> get_token(resource)
      |> do_notify(user_id, resource, message)
    end
    :ok
  end

  @spec notify_all(Wocky.User.id, any) :: :ok
  def notify_all(user_id, event) do
    if enabled?() do
      message = Event.format(event)
      for {resource, token} <- get_all_tokens(user_id) do
        do_notify(token, user_id, resource, message)
      end
    end
    :ok
  end

  # ===================================================================
  # Helpers

  defp enabled? do
    Confex.get_env(:wocky, Wocky.Push)[:enabled]
  end

  defp get_token(user_id, resource) do
    Repo.one(
      from t in Token,
        where: [user_id: ^user_id, resource: ^resource, valid: true],
        select: t.token
    )
  end

  defp get_all_tokens(user_id) do
    Repo.all(
      from t in Token,
        where: [user_id: ^user_id, valid: true],
        select: {t.resource, t.token}
    )
  end

  defp do_notify(nil, user_id, resource, _message) do
    Logger.error "Attempted to send notification to user " <>
      "#{user_id}/#{resource} but they have no token."
  end
  defp do_notify(token, user_id, resource, message) do
    message
    |> maybe_truncate_message()
    |> make_payload(token)
    |> maybe_push(sandbox?())
    |> handle_response(user_id, resource)
  end

  defp maybe_truncate_message(message) do
    if byte_size(message) > @message_limit do
      String.slice(message, 0, @message_limit - 3) <> "..."
    else
      message
    end
  end

  defp make_payload(message, token) do
    message
    |> Notification.new(token)
    |> Notification.put_badge(1)
  end

  defp maybe_push(n, false), do: APNS.push(n)
  defp maybe_push(n, true) do
    notif = if n.payload["aps"]["alert"] == "bad token" do
      %Notification{n | id: "testing", response: :bad_device_token}
    else
      %Notification{n | id: "testing", response: :success}
    end
    Sandbox.record_notification(notif, self())
    send(self(), notif)
    notif
  end

  defp sandbox? do
    Confex.get_env(:wocky, Wocky.Push)[:sandbox]
  end

  defp handle_response(%Notification{response: resp} = n, user_id, resource) do
    if resp == :bad_device_token do
      Logger.error "Bad device token for user #{user_id}/#{resource}."
      invalidate_token(user_id, resource, n.device_token)
    end

    do_db_log(user_id, resource, n)
  end

  defp invalidate_token(user_id, resource, token) do
    Repo.update_all(
      from(Token,
           where: [user_id: ^user_id, resource: ^resource, token: ^token]),
      set: [valid: false, invalidated_at: DateTime.utc_now]
    )
  end

  defp do_db_log(user_id, resource, %Notification{} = n) do
    %{user_id: user_id,
      resource: resource,
      token: n.device_token,
      message_id: n.id,
      payload: inspect(n.payload),
      response: to_string(n.response),
      details: Error.msg(n.response)}
    |> Log.insert_changeset
    |> Repo.insert!
  end
end
