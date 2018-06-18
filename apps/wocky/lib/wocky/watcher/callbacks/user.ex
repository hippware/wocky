defmodule Wocky.Watcher.Callbacks.User do
  @moduledoc """
  Callbacks for DB user changes
  """

  alias Wocky.User
  alias Wocky.User.GeoFence
  alias Wocky.Watcher.Client
  alias WockyDBWatcher.Event

  def register do
    Client.subscribe(User, :update, &handle_update/1)
  end

  # No change to hidden timeout - nothing to do
  def handle_update(%Event{action: :update, old: old, new: new}) do
    handle_hidden(new, User.hidden?(old), User.hidden?(new))
  end

  # Newly hidden
  defp handle_hidden(user, false, true) do
    GeoFence.exit_all_bots(user)
  end

  defp handle_hidden(_, _, _), do: :ok
end
