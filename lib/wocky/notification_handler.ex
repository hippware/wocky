defmodule Wocky.PushNotificationHandler do
  @moduledoc """
  """

  require Logger

  use GenStage

  alias Wocky.JID
  alias Wocky.PushNotification

  def start_link() do
    {:ok, handler_type} =
      :application.get_env(:wocky, :notification_handler, :none)
    handler = case handler_type do
        :aws ->
            :ok = Logger.info("AWS Notifications enabled")
            Wocky.Notification.AWSHandler;
        :test ->
            :ok = Logger.info("Notification testing system enabled")
            Wocky.Notification.TestHandler;
        :none ->
            :ok = Logger.info("Notifications disabled")
            Wocky.Notification.NullHandler
    end
    GenStage.start_link(__MODULE__, handler)
  end

  ## Callbacks

  def init(handler) do
    handler.init()
    # Starts a permanent subscription to the broadcaster
    # which will automatically start requesting items.
    {:consumer, handler, subscribe_to: [PushNotification]}
  end

  def handle_events(events, _from, handler) do
    _ = Task.async_stream(events, fn(e) -> handle_event(e, handler) end)
    {:noreply, [], handler}
  end

  defp handle_event(
    %PushNotification{to: to, from: nil, body: body}, handler) do
    to
    |> all_endpoints
    |> Enum.each(fn(ep) -> do_notify(ep, nil, body, handler) end)
  end

  defp handle_event(
    %PushNotification{to: to, from: from, body: body}, handler) do
    to
    |> all_endpoints
    |> Enum.each(fn(ep) -> do_notify(ep, from, body, handler) end)
  end

  defp all_endpoints(jid) do
    {user, server} = JID.to_lus(jid)
    :wocky_db.select_column(server, :device, :endpoint, %{user => user,
                                                          server => server})
  end

  defp do_notify(endpoint, from, body, handler) do
    endpoint
    |> handler.notify(from, body)
    |> log_result(endpoint, from, body)
  end

  defp log_result({:error, e}, to, from, message) do
    Logger.warn("Notification error '#{inspect(e)}' while sending "
                <> "#{from} => #{to} with body '#{message}'")
  end

  defp log_result(:ok, to, from, message) do
    Logger.debug("Notification sent: "
                 <> "#{from} => #{to} with body '#{message}'")
  end
end

