defmodule Wocky.Push do
  @moduledoc """
  The Push context. Single interface for push notifications.
  """
  use Elixometer

  import Ecto.Query, warn: false

  alias Pigeon.APNS
  alias Pigeon.APNS.{Error, Notification}
  alias Wocky.Push.{Event, Log, Sandbox, Token}
  alias Wocky.Repo
  alias Wocky.User

  require Logger

  @type message :: binary

  @message_limit 512

  @max_retries 5

  defstruct [
    :token,
    :user,
    :device,
    :event,
    retries: 0,
    resp: nil
  ]

  @type t :: %__MODULE__{
          token: binary(),
          user: User.t(),
          device: User.device(),
          event: Event.t(),
          retries: non_neg_integer(),
          resp: Notification.response()
        }

  # ===================================================================
  # Push Token API

  @spec enable(
          User.id(),
          User.device(),
          Token.token(),
          binary | nil,
          boolean | nil
        ) :: :ok
  def enable(user_id, device, token, platform \\ nil, dev_mode \\ nil) do
    %{
      user_id: user_id,
      device: device,
      token: token,
      platform: platform,
      dev_mode: dev_mode
    }
    |> Token.register_changeset()
    |> Repo.insert!(
      on_conflict: [set: conflict_updates(dev_mode)],
      conflict_target: [:user_id, :device, :token]
    )

    Token
    |> where([t], t.user_id == ^user_id)
    |> where([t], t.device == ^device)
    |> where([t], t.token != ^token)
    |> Repo.update_all(set: [valid: false, disabled_at: DateTime.utc_now()])

    :ok
  end

  defp conflict_updates, do: [valid: true, enabled_at: DateTime.utc_now()]

  defp conflict_updates(nil), do: conflict_updates()

  defp conflict_updates(dev_mode) do
    [dev_mode: dev_mode] ++ conflict_updates()
  end

  @spec disable(Wocky.User.id(), Wocky.User.device()) :: :ok
  def disable(user_id, device) do
    Repo.update_all(
      from(Token, where: [user_id: ^user_id, device: ^device, valid: true]),
      set: [valid: false, disabled_at: DateTime.utc_now()]
    )

    :ok
  end

  @spec purge(Wocky.User.id()) :: :ok
  def purge(user_id) do
    Repo.delete_all(from Token, where: [user_id: ^user_id])

    :ok
  end

  # ===================================================================
  # Push Notification API

  @spec notify(Wocky.User.t(), Wocky.User.device(), any) :: :ok
  def notify(user, device, event) do
    if enabled?() do
      token = get_token(user.id, device)

      do_notify(%__MODULE__{
        user: user,
        device: device,
        token: token,
        event: event
      })
    end

    :ok
  end

  @spec notify_all(Wocky.User.t(), any) :: :ok
  def notify_all(user, event) do
    if enabled?() do
      for {device, token} <- get_all_tokens(user.id) do
        do_notify(%__MODULE__{
          token: token,
          user: user,
          device: device,
          event: event
        })
      end
    end

    :ok
  end

  # ===================================================================
  # Helpers

  defp enabled? do
    Confex.get_env(:wocky, Wocky.Push)[:enabled]
  end

  defp get_token(user_id, device) do
    Repo.one(
      from t in Token,
        where: [user_id: ^user_id, device: ^device, valid: true],
        select: t.token
    )
  end

  defp get_all_tokens(user_id) do
    Repo.all(
      from t in Token,
        where: [user_id: ^user_id, valid: true],
        select: {t.device, t.token}
    )
  end

  defp do_notify(%__MODULE__{token: nil, user: user, device: device}) do
    Logger.error(
      "Attempted to send notification to user " <>
        "#{user.id}/#{device} but they have no token."
    )
  end

  defp do_notify(%__MODULE__{resp: resp, retries: @max_retries} = params) do
    log_failure(params)
    send_honeybadger(Error.msg(resp))
  end

  defp do_notify(
         %__MODULE__{token: token, event: event, retries: retries} = params
       ) do
    # Don't start_link here - we want the timeout to fire even if we crash
    {:ok, timeout_pid} = Task.start(fn -> push_timeout(params) end)

    on_response = fn r -> handle_response(r, timeout_pid, params) end

    event
    |> make_payload(token)
    |> maybe_push(on_response, retries, sandbox?())
  end

  defp maybe_truncate_message(message) do
    if byte_size(message) > @message_limit do
      String.slice(message, 0, @message_limit - 3) <> "..."
    else
      message
    end
  end

  defp make_payload(event, token) do
    uri = Event.uri(event)

    event
    |> Event.message()
    |> maybe_truncate_message()
    |> Notification.new(token, topic())
    |> Notification.put_badge(1)
    |> Notification.put_custom(%{"uri" => uri})
  end

  defp maybe_push(n, on_response, _, false) do
    APNS.push(n, on_response: on_response, timeout: timeout())
  end

  defp maybe_push(n, on_response, retries, true) do
    notif =
      case n.payload["aps"]["alert"] do
        "bad token" ->
          %Notification{n | id: "testing", response: :bad_device_token}

        "raise" ->
          raise "requested_raise"

        "retry test" when retries < 2 ->
          %Notification{n | id: "testing", response: :failed}

        _ ->
          %Notification{n | id: "testing", response: :success}
      end

    Sandbox.record_notification(notif, self())

    if reflect?(), do: send(self(), notif)

    on_response.(notif)

    notif
  end

  defp sandbox?, do: get_conf(:sandbox)

  defp reflect?, do: get_conf(:reflect)

  defp topic, do: get_conf(:topic)

  defp timeout, do: get_conf(:timeout)

  defp get_conf(key), do: Confex.get_env(:wocky, Wocky.Push)[key]

  defp handle_response(
         %Notification{response: resp} = n,
         timeout_pid,
         %__MODULE__{user: user, device: device} = params
       ) do
    send(timeout_pid, :push_complete)
    update_metric(resp)
    do_db_log(n, user, device)
    maybe_handle_error(n, %{params | resp: resp})
  end

  defp maybe_handle_error(_n, %__MODULE__{resp: :success}), do: :ok

  defp maybe_handle_error(
         n,
         %__MODULE__{
           user: user,
           device: device,
           retries: retries,
           resp: resp
         } = params
       ) do
    Logger.error("PN Error: #{Error.msg(resp)}")

    if resp == :bad_device_token do
      invalidate_token(user.id, device, n.device_token)
    else
      do_notify(%{params | retries: retries + 1})
    end
  end

  defp send_honeybadger(message) do
    raise RuntimeError, message
  rescue
    exception ->
      Honeybadger.notify(exception)
  end

  defp invalidate_token(user_id, device, token) do
    Repo.update_all(
      from(
        Token,
        where: [user_id: ^user_id, device: ^device, token: ^token]
      ),
      set: [valid: false, invalidated_at: DateTime.utc_now()]
    )
  end

  defp update_metric(resp),
    do: update_counter("push_notfications.#{to_string(resp)}", 1)

  defp do_db_log(%Notification{} = n, user, device) do
    %{
      user_id: user.id,
      device: device,
      token: n.device_token,
      message_id: n.id,
      payload: inspect(n.payload),
      response: to_string(n.response),
      details: Error.msg(n.response)
    }
    |> Log.insert_changeset()
    |> Repo.insert!()
  end

  defp push_timeout(%__MODULE__{retries: retries} = params) do
    timeout = timeout() * 2

    receive do
      :push_complete -> :ok
    after
      timeout ->
        log_timeout(params)
        do_notify(%{params | retries: retries + 1})
    end
  end

  defp log_timeout(%__MODULE__{
         token: token,
         user: user,
         device: device,
         event: event
       }) do
    Logger.error("PN Error: timeout expired")

    %{
      user_id: user.id,
      device: device,
      token: token,
      message_id: nil,
      payload: Event.message(event),
      response: "timeout",
      details: "Timeout waiting for response from Pigeon"
    }
    |> Log.insert_changeset()
    |> Repo.insert!()
  end

  defp log_failure(%__MODULE__{
         token: token,
         user: user,
         device: device,
         event: event
       }) do
    %{
      user_id: user.id,
      device: device,
      token: token,
      message_id: nil,
      payload: Event.message(event),
      response: "max retries reached",
      details:
        "Maximum number of #{@max_retries} retries sending push notification."
    }
    |> Log.insert_changeset()
    |> Repo.insert!()
  end
end
