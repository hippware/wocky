defmodule Wocky.Notification.TestHandler do
  @moduledoc """
  Implements a testing `wocky_notification_handler`. All notifications are
  recorded, but no further action occurs. They can then be queried by the
  testing system.
  """

  @behaviour :wocky_notification_handler

  use Exref, ignore: [get_notifications: 0]

  @spec init :: :ok
  def init do
    _ = if :ets.info(:test_handler_table) == :undefined do
      :ets.new(:test_handler_table, [:public, :named_table, :duplicate_bag])
    end
    :ets.delete_all_objects(:test_handler_table)
    :ok
  end

  @spec register(binary, binary, binary) :: {:ok, binary}
  def register(_user, _platform, device) do
    {:ok, device}
  end

  @spec notify(binary, binary, binary) :: :ok
  def notify(endpoint, from, body) do
    :ets.insert(:test_handler_table, {endpoint, from, body})
    :ok
  end

  @spec get_notifications :: [{binary, binary} | {binary, binary, binary}]
  def get_notifications do
    list = :ets.tab2list(:test_handler_table)
    :ets.delete_all_objects(:test_handler_table)
    list
  end

end
