defmodule Wocky.PushHelper do
  @moduledoc """
  Module to set up the db watcher/callback system for test cases that require it
  """

  alias Wocky.Notifier.Push.Backend.Sandbox

  @spec no_more_push_notifications :: boolean()
  def no_more_push_notifications do
    msgs = Sandbox.wait_notifications(global: true)
    Enum.empty?(msgs)
  end

  @spec clear_expected_notifications(non_neg_integer()) :: boolean()
  def clear_expected_notifications(count) do
    result =
      length(
        Sandbox.wait_notifications(global: true, timeout: 500, count: count)
      ) == count

    Sandbox.clear_notifications(global: true)
    result
  end
end
