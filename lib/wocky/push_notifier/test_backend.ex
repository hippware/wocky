defmodule Wocky.PushNotifier.TestBackend do
  @moduledoc """
  Implements a testing `PushNotifier`. All notifications are recorded,
  but no further action occurs. They can then be queried by the testing system.
  """

  @behaviour Wocky.PushNotifier

  def init do
    if :ets.info(:test_handler_table) == :undefined do
      _ = :ets.new(:test_handler_table, [:public, :named_table, :duplicate_bag])
    end
    :ets.delete_all_objects(:test_handler_table)
    :ok
  end

  def enable(_user, _server, _resource, _platform, device) do
    {:ok, device}
  end

  def disable(_endpoint) do
    :ok
  end

  def push(body, endpoint) do
    :ets.insert(:test_handler_table, {endpoint, body})
    :ok
  end

  @spec get_notifications :: [{binary, binary} | {binary, binary, binary}]
  def get_notifications do
    list = :ets.tab2list(:test_handler_table)
    :ets.delete_all_objects(:test_handler_table)
    list
  end
end
