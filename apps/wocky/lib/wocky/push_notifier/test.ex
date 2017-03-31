defmodule Wocky.PushNotifier.Test do
  @moduledoc """
  Implements a testing `PushNotifier`. All push notifications are
  recorded, but no further action occurs. They can then be queried by the
  testing system.
  """

  @behaviour Wocky.PushNotifier

  @spec init :: :ok
  def init do
    _ = if :ets.info(:test_handler_table) == :undefined do
      :ets.new(:test_handler_table, [:public, :named_table, :duplicate_bag])
    end
    :ets.delete_all_objects(:test_handler_table)
    :ok
  end

  @spec register(binary, binary, binary, binary) :: {:ok, binary}
  def register(_user, _resource, _platform, device) do
    {:ok, device}
  end

  @spec push(binary, binary) :: :ok
  def push(endpoint, message) do
    :ets.insert(:test_handler_table, {endpoint, message})
    :ok
  end

  @spec get_notifications :: [{binary, binary}]
  def get_notifications do
    list = :ets.tab2list(:test_handler_table)
    :ets.delete_all_objects(:test_handler_table)
    list
  end
end
