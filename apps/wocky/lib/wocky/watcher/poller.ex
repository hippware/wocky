defmodule Wocky.Watcher.Poller do
  @moduledoc """
  A simple module that turns a blocking "pull" event source into a "push" one
  that sends to another process.
  """

  def start_link(source, send_to) do
    Task.start_link(fn -> poll(source, send_to) end)
  end

  def poll(source, send_to) do
    {:ok, messages} = source.recv()

    messages
    |> Enum.map(fn(m) -> m.body end)
    |> send_to.send()

    source.delete(messages)
    poll(source, send_to)
  end
end
