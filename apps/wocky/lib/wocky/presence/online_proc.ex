defmodule Wocky.Presence.OnlineProc do
  @moduledoc """
  Trivial process that exists when a user is online and doesn't when they're
  not. Allows other users to monitor this process to see when a user goes
  offline
  """

  @spec start_link :: {:ok, pid()}
  def start_link do
    self = self()
    Task.start_link(fn -> init(self) end)
  end

  @spec go_offline(pid()) :: :ok
  def go_offline(pid) do
    send(pid, :go_offline)
    :ok
  end

  defp init(parent) do
    parent
    |> Process.monitor()
    |> loop()
  end

  defp loop(ref) do
    receive do
      {:DOWN, ^ref, :process, _, _} ->
        :ok

      :go_offline ->
        :ok

      _ ->
        loop(ref)
    end
  end
end
