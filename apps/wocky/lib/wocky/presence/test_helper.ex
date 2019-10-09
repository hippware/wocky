defmodule Wocky.Presence.TestHelper do
  @moduledoc false

  # Helper functions for testing presence.
  # Not intended for production use.
  #
  alias Wocky.Presence

  @doc false
  def connect(user) do
    self = self()

    conn_pid =
      spawn_link(fn ->
        users = Presence.connect(user)
        send(self, {:connected, users})

        receive do
          :exit -> :ok
        end
      end)

    receive do
      {:connected, users} -> {conn_pid, users}
    end
  end

  @doc false
  def disconnect(pid), do: send(pid, :exit)
end
