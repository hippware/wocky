defmodule Wocky.PushNotifier.TestBackend do
  @moduledoc """
  Implements a testing `PushNotifier`. All notifications are recorded and the
  call is logged, but no further action occurs. The notifications can then be
  queried by the testing system.
  """

  use Wocky.JID

  require Logger

  @behaviour Wocky.PushNotifier

  def init do
    Logger.info("Notification testing system enabled")
    if :ets.info(:test_registrations_table) == :undefined do
      _ = :ets.new(:test_registrations_table, [:public, :named_table, :set])
    end
    if :ets.info(:test_notifications_table) == :undefined do
      _ = :ets.new(:test_notifications_table,
                   [:public, :named_table, :duplicate_bag])
    end
    reset()
    :ok
  end

  def enable(_, _, _, _, "error"), do: {:error, :testing}
  def enable(user, server, resource, platform, device) do
    jid = user |> JID.make(server, resource) |> JID.to_binary
    Logger.info("""
    Notification registration request for #{jid} with device '#{device}'\
    """)
    endpoint = Base.encode64(jid)
    :ets.insert(:test_registrations_table, {endpoint, jid, platform, device})
    {:ok, endpoint}
  end

  def disable(endpoint) do
    Logger.info("Disabling endpoint '#{endpoint}'")
    :ets.delete(:test_registrations_table, endpoint)
    :ok
  end

  def push("error", _), do: {:error, :testing}
  def push(body, endpoint) do
    Logger.info("Notification to '#{endpoint}' with body: #{body}")
    :ets.insert(:test_notifications_table, {endpoint, body})
    :ok
  end

  @spec get_registrations :: [{binary, binary, binary, binary}]
  def get_registrations do
    :ets.tab2list(:test_registrations_table)
  end

  @spec get_notifications :: [{binary, binary}]
  def get_notifications do
    :ets.tab2list(:test_notifications_table)
  end

  @spec reset :: :ok
  def reset do
    :ets.delete_all_objects(:test_registrations_table)
    :ets.delete_all_objects(:test_notifications_table)
    :ok
  end
end
