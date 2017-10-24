defmodule Wocky.PushEventHandler do
  @moduledoc "Event handler for Pushex push notifications"

  use Pushex.EventHandler

  alias APNS.Error
  alias APNS.Feedback
  alias Pushex.APNS.Response
  alias Wocky.Device
  alias Wocky.NotificationLog

  require Logger

  def handle_event({:request, request, _info}, state) do
    Logger.info "Sending request #{inspect request}"
    {:ok, state}
  end

  def handle_event({:response, {:ok, response}, request, info}, state) do
    handle_event({:response, response, request, info}, state)
  end

  def handle_event({:response, %Response{} = response,
                   request, {_, reference}}, state) do
    if response.success >= 1 do
      Logger.info """
      Sent notification to device #{request.to} with content \
      #{inspect request.notification}\
      """
      if log_notifications?(), do: NotificationLog.result(reference, true)
    else
      Logger.warn """
      Failed to send notification to device #{request.to} with content \
      #{inspect request.notification}: #{inspect response.results}\
      """
      if log_notifications?(),
        do: NotificationLog.result(reference, false, inspect response.results)
    end
    {:ok, state}
  end

  def handle_event({:error, %Error{status: 8}, token}, state) do
    Logger.warn "Received validation error for token #{token}; invalidating."
    Device.invalidate(token)
    {:ok, state}
  end

  def handle_event({:error, error, token}, state) do
    Logger.error "Received error for token #{token}: #{error.error}"
    {:ok, state}
  end

  def handle_event({:feedback, %Feedback{time: utime, token: token}}, state) do
    Logger.warn "Received feedback on token #{token}; invalidating."
    time = Timex.from_unix(utime)
    device = Device.get_by_token(token)
    if device && !device.invalid && device.updated_at < time do
      Device.invalidate(token, true)
    end
    {:ok, state}
  end

  def handle_event(event, state) do
    Logger.error "Unknown push notification event: #{inspect event}"
    {:ok, state}
  end

  defp log_notifications?, do: Confex.get_env(:wocky, :log_push_notifications)
end
