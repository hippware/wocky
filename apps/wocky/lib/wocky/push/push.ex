defmodule Wocky.Push do
  @moduledoc """
  The Push context. Single interface for push notifications.
  """
  use Elixometer

  import Ecto.Query, warn: false

  alias Pigeon.APNS.Notification, as: APNSNotification
  alias Pigeon.FCM.Notification, as: FCMNotification
  alias Wocky.Push.{Event, Log, Token}
  # FCM
  alias Wocky.Push.Backend.{APNS, Sandbox}
  alias Wocky.Repo
  alias Wocky.User

  require Logger

  @message_limit 512

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
  @type response :: APNSNotification.response() | FCMNotification.status()
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

  @spec notify_all(Wocky.User.t(), any) :: :ok
  def notify_all(user, event) do
    if enabled?() do
      for token <- Token.all_for_user(user) do
        platform = token.platform

        backend =
          cond do
            sandbox?() -> Sandbox
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
    end

    :ok
  end

  # ===================================================================
  # Helpers

  def get_conf(key), do: Confex.get_env(:wocky, Wocky.Push)[key]

  defp sandbox?, do: get_conf(:sandbox)

  def reflect?, do: get_conf(:reflect)

  def topic, do: get_conf(:topic)

  def timeout, do: get_conf(:timeout)

  defp enabled?, do: get_conf(:enabled)

  defp log_payload?, do: get_conf(:log_payload)

  def do_notify(%__MODULE__{token: nil, user: user, device: device}) do
    Logger.error(
      "PN Error: Attempted to send notification to user " <>
        "#{user.id}/#{device} but they have no token."
    )
  end

  def do_notify(
        %__MODULE__{backend: backend, resp: resp, retries: @max_retries} =
          params
      ) do
    log_failure(params)
    Logger.error("PN Error: #{backend.error_msg(resp)}")
  end

  def do_notify(%__MODULE__{backend: backend} = params) do
    # Don't start_link here - we want the timeout to fire even if we crash
    {:ok, timeout_pid} = Task.start(fn -> push_timeout(params) end)

    on_response = fn r -> backend.handle_response(r, timeout_pid, params) end

    params
    |> Map.put(:backend, backend)
    |> Map.put(:on_response, on_response)
    |> backend.push()
  end

  def maybe_truncate_message(message) do
    if byte_size(message) > @message_limit do
      String.slice(message, 0, @message_limit - 3) <> "..."
    else
      message
    end
  end

  def invalidate_token(user_id, device, token) do
    Repo.update_all(
      from(
        Token,
        where: [user_id: ^user_id, device: ^device, token: ^token]
      ),
      set: [valid: false, invalidated_at: DateTime.utc_now()]
    )
  end

  def update_metric(resp),
    do: update_counter("push_notfications.#{to_string(resp)}", 1)

  def db_log(msg) do
    msg
    |> Log.insert_changeset()
    |> Repo.insert!()
  end

  def maybe_extract_payload(payload, user) do
    if User.hippware?(user) || log_payload?() do
      inspect(payload)
    end
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
