defmodule Wocky.Notifier.Push do
  @moduledoc """
  The Push context. Single interface for push notifications.
  """
  use Elixometer

  import Ecto.Query, warn: false

  alias Pigeon.APNS.Notification, as: APNSNotification
  alias Pigeon.FCM.Notification, as: FCMNotification
  alias Wocky.Notifier.Push.{Event, Log, Token, Utils}
  alias Wocky.Notifier.Push.Backend.{APNS, FCM, Sandbox}
  alias Wocky.Repo
  alias Wocky.User

  require Logger

  @behaviour Wocky.Notifier

  @max_retries 5

  defstruct [
    :token,
    :user,
    :device,
    :platform,
    :event,
    :backend,
    :on_response,
    retries: 0,
    resp: nil
  ]

  @type message :: binary
  @type notification :: APNSNotification.t() | FCMNotification.t()
  @type id :: String.t() | nil
  @type payload :: map()
  @type response ::
          APNSNotification.response()
          | FCMNotification.status()
          | FCMNotification.regid_error_response()
  @type on_response :: (notification() -> no_return)

  @type t :: %__MODULE__{
          token: Token.token(),
          user: User.t(),
          device: User.device(),
          platform: Token.PushServicePlatform.t(),
          event: Event.t(),
          backend: module(),
          on_response: on_response(),
          retries: non_neg_integer(),
          resp: response()
        }

  # ===================================================================
  # Push Token API

  @spec enable(
          User.t(),
          User.device(),
          Token.token(),
          binary | nil,
          boolean | nil
        ) :: :ok
  def enable(user, device, token, platform \\ nil, dev_mode \\ nil) do
    %{
      user_id: user.id,
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
    |> where([t], t.user_id == ^user.id)
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

  @spec disable(Wocky.User.t(), Wocky.User.device()) :: :ok
  def disable(user, device) do
    Repo.update_all(
      from(Token, where: [user_id: ^user.id, device: ^device, valid: true]),
      set: [valid: false, disabled_at: DateTime.utc_now()]
    )

    :ok
  end

  # ===================================================================
  # Push Notification API

  @impl true
  def notify(event) do
    notify_all(Event.recipient(event), event)
  end

  @spec notify_all(Wocky.User.t(), any) :: :ok
  def notify_all(user, event) do
    if Utils.enabled?() do
      for token <- Token.all_for_user(user) do
        platform = token.platform

        backend =
          cond do
            Utils.sandbox?() -> Sandbox
            platform == :apns -> APNS
            platform == :fcm -> FCM
          end

        do_notify(%__MODULE__{
          token: token.token,
          user: user,
          device: token.device,
          platform: platform,
          backend: backend,
          event: event
        })
      end

      :ok
    else
      :ok
    end
  end

  # ===================================================================
  # Helpers

  defp do_notify(%__MODULE__{token: nil, user: user, device: device}) do
    Logger.error(
      "PN Error: Attempted to send notification to user " <>
        "#{user.id}/#{device} but they have no token."
    )
  end

  defp do_notify(
         %__MODULE__{backend: backend, resp: resp, retries: @max_retries} =
           params
       ) do
    log_failure(params)
    Logger.error("PN Error: #{backend.error_msg(resp)}")
  end

  defp do_notify(%__MODULE__{backend: backend} = params) do
    # Don't start_link here - we want the timeout to fire even if we crash
    {:ok, timeout_pid} = Task.start(fn -> push_timeout(params) end)

    on_response = fn r -> handle_response(r, timeout_pid, params) end

    params
    |> Map.put(:backend, backend)
    |> Map.put(:on_response, on_response)
    |> backend.push()
  end

  def handle_response(notification, timeout_pid, params) do
    send(timeout_pid, :push_complete)
    resp = params.backend.get_response(notification)
    update_metric(resp)
    db_log(log_msg(notification, params))
    maybe_handle_error(%{params | resp: resp})
  end

  defp maybe_handle_error(%__MODULE__{resp: :success}), do: :ok

  defp maybe_handle_error(
         %__MODULE__{
           backend: backend,
           user: user,
           device: device,
           retries: retries,
           token: token,
           resp: resp
         } = params
       ) do
    _ = Logger.error("PN Error: #{backend.error_msg(resp)}")

    case backend.handle_error(resp) do
      :retry -> do_notify(%{params | retries: retries + 1})
      :invalidate_token -> invalidate_token(user.id, device, token)
    end
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

  defp db_log(msg) do
    msg
    |> Log.insert_changeset()
    |> Repo.insert!()
  end

  defp maybe_extract_payload(payload, user) do
    if User.hippware?(user) || Utils.log_payload?() do
      inspect(payload)
    end
  end

  defp push_timeout(%__MODULE__{retries: retries} = params) do
    timeout = Utils.timeout() * 2

    receive do
      :push_complete -> :ok
    after
      timeout ->
        log_timeout(params)
        do_notify(%{params | retries: retries + 1})
    end
  end

  defp log_msg(n, %__MODULE__{
         user: user,
         device: device,
         token: token,
         backend: backend
       }) do
    resp = backend.get_response(n)

    %{
      user_id: user.id,
      device: device,
      token: token,
      message_id: backend.get_id(n),
      payload: maybe_extract_payload(backend.get_payload(n), user),
      response: to_string(resp),
      details: backend.error_msg(resp)
    }
  end

  defp log_timeout(%__MODULE__{
         token: token,
         user: user,
         device: device,
         event: event
       }) do
    _ = Logger.error("PN Error: timeout expired")

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
