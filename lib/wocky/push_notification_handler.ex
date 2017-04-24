defmodule Wocky.PushNotificationHandler do
  @moduledoc """
  """

  require Logger

  use GenStage

  alias Wocky.JID
  alias Wocky.PushNotification

  @spec start_link(Keyword.t) :: {:ok, pid}
  def start_link(cfg_terms) do
    handler_type = Keyword.get(cfg_terms, :notification_system, :none)
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
    :application.set_env(:wocky, :push_notification_handler, handler)
    GenStage.start_link(__MODULE__, handler)
  end

  ## Callbacks

  @spec init(module) :: {:consumer, module, Keyword.t}
  def init(handler) do
    :ok = handler.init()
    # Starts a permanent subscription to the broadcaster
    # which will automatically start requesting items.
    {:consumer, handler, subscribe_to: [PushNotifications]}
  end

  @spec handle_events(list, term, module) :: {:noreply, [], module}
  def handle_events(events, _from, handler) do
    _ = events
    |> Task.async_stream(fn(e) -> handle_event(e, handler) end)
    |> Enum.to_list
    {:noreply, [], handler}
  end

  defp handle_event(
    %PushNotification{to: to, from: from, body: body}, handler) do
    to
    |> all_endpoints
    |> Enum.each(fn(ep) -> do_notify(ep, from, body, handler) end)
  end

  defp all_endpoints(jid) do
    {user, server} = JID.to_lus(jid)
    :wocky_db.select_column(server, :device, :endpoint, %{user: user,
                                                          server: server})
  end

  defp do_notify(endpoint, from, body, handler) do
    endpoint
    |> handler.notify(from, body)
    |> log_result(endpoint, from, body)
  end

  defp log_result({:error, e}, endpoint, from, message) do
    Logger.warn("""
                Notification error '#{inspect(e)}' while sending
                #{JID.to_binary(from)} => #{endpoint}
                with body '#{message}'
                """)
  end

  defp log_result(:ok, endpoint, from, message) do
    Logger.debug("""
                 Notification sent:
                 #{JID.to_binary(from)} => #{endpoint}
                 with body '#{message}'
                 """)
  end
end

