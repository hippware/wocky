defmodule Wocky.PushEventHandler do
  @moduledoc "Event handler for Pushex push notifications"

  use Pushex.EventHandler

  alias Pushex.APNS.Response
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
      NotificationLog.result(reference, true)
    else
      Logger.warn """
      Failed to send notification to device #{request.to} with content \
      #{inspect request.notification}: #{inspect response.results}\
      """
      NotificationLog.result(reference, false, inspect response.results)
    end
    {:ok, state}
  end

  def handle_event({:error, error, token}, state) do
    Logger.error "Unknown error for token #{token}: #{inspect error}"
    {:ok, state}
  end

  def handle_event({:feedback, feedback}, state) do
    Logger.warn "Feedback on push operation: #{inspect feedback}"
    {:ok, state}
  end

  def handle_event(event, state) do
    Logger.error "Unknown push notification event: #{inspect event}"
    {:ok, state}
  end
end
