defmodule Wocky.Notifier.Push.Backend.FCM do
  @moduledoc """
  Firebase Cloud Messaging (Android) implementation for wocky push system
  """

  @behaviour Wocky.Notifier.Push.Backend

  use ModuleConfig, otp_app: :wocky

  alias Pigeon.FCM
  alias Pigeon.FCM.Notification
  alias Wocky.Notifier.Push
  alias Wocky.Notifier.Push.Event
  alias Wocky.Notifier.Push.Token
  alias Wocky.Notifier.Push.Utils

  @impl true
  def push(params) do
    _ =
      params.event
      |> build_notification(params.token)
      |> FCM.push(
        on_response: params.on_response,
        timeout: Push.get_config(:timeout)
      )

    :ok
  end

  @spec build_notification(Event.t(), Token.token()) :: Notification.t()
  def build_notification(event, token) do
    background = Keyword.get(Event.opts(event), :background, false)

    token
    |> build_notification(event, background)
    |> Notification.put_restricted_package_name(package())
    |> Notification.put_priority(Event.opts(event)[:priority])
  end

  @spec build_notification(Token.token(), Event.t(), boolean()) ::
          Notification.t()
  # Foreground notification
  def build_notification(token, event, false) do
    opts = Event.opts(event)
    sound = Keyword.get(opts, :sound, "silence")

    event_msg =
      event
      |> Event.message()
      |> Utils.maybe_truncate_message()

    body =
      maybe_add_channel_id(
        %{"title" => "tinyrobot", "body" => event_msg, "sound" => sound},
        opts[:android_channel_id]
      )

    Notification.new(token, body, %{"url" => Event.uri(event)})
  end

  # Background notification
  def build_notification(token, event, true) do
    data = Event.opts(event)[:extra_fields] || %{}
    Notification.new(token, %{}, data)
  end

  defp maybe_add_channel_id(body, nil), do: body

  defp maybe_add_channel_id(body, channel_id),
    do: Map.put(body, "android_channel_id", channel_id)

  @impl true
  def get_response(%Notification{status: :success, response: response}),
    do: get_response(response)

  def get_response(%Notification{status: status}), do: status

  def get_response([{:update, _regids}]), do: :success
  def get_response([{response, _regid}]), do: response

  @impl true
  def get_id(%Notification{message_id: id}), do: id

  @impl true
  def get_payload(%Notification{payload: payload}), do: payload

  @impl true
  def handle_error(:not_registered), do: :invalidate_token
  def handle_error(:invalid_registration), do: :invalidate_token
  def handle_error(:missing_registration), do: :invalidate_token
  def handle_error(_), do: :retry

  @impl true
  def error_msg(resp), do: inspect(resp)

  defp package, do: get_config(:package)
end
