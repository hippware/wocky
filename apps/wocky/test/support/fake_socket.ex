defmodule Wocky.Test.FakeSocket do
  @moduledoc """
  A process pretending to be a websocket which has registered with the presence
  manager as belonging to a particular user. Used in testing when we want to
  test socket registration without the rigmorole of setting up a full websocket.
  """

  alias Wocky.Account.User
  alias Wocky.Presence

  @spec open(User.t()) :: pid()
  def open(user) do
    self = self()

    pid =
      spawn_link(fn ->
        ref = Process.monitor(self)

        receive do
          :exit -> :ok
          {:DOWN, ^ref, :process, _, _} -> :ok
        end
      end)

    Presence.register_socket(user, pid)
    pid
  end

  @spec close(pid()) :: :ok
  def close(pid) do
    send(pid, :exit)
    ref = Process.monitor(pid)

    receive do
      {:DOWN, ^ref, :process, _, _} -> :ok
    end
  end
end
