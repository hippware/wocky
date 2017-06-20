defmodule Wocky.PushEventHandler do
  @moduledoc "Event handler for Pushex push notifications"

  use GenEvent

  alias Pushex.APNS.Response

  require Logger

  def handle_event({:request, request, _info}, state) do
    Logger.info "Sending request #{inspect request}"
    {:ok, state}
  end

  def handle_event({:response, {:ok, response}, request, info}, state) do
    handle_event({:response, response, request, info}, state)
  end

  def handle_event({:response, %Response{} = response, request, _}, state) do
    if response.success >= 1 do
      Logger.info """
      Sent notification to device #{request.to} with content \
      #{inspect request.notification}\
      """
    else
      Logger.warn """
      Failed to send notification to device #{request.to} with content \
      #{inspect request.notification}: #{inspect response.results}\
      """
    end
    {:ok, state}
  end

  def handle_event({:error, error, token}, state) do
    Logger.info "Unknown error for token #{token}: #{inspect error}"
    {:ok, state}
  end

  def handle_event({:feedback, _feedback}, state), do: {:ok, state}
end
