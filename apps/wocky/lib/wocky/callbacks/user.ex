defmodule Wocky.Callbacks.User do
  @moduledoc """
  Callbacks for DB user changes
  """

  use Wocky.Watcher, type: Wocky.User, events: [:update]

  alias Wocky.User
  alias Wocky.User.GeoFence

  # No change to hidden timeout - nothing to do
  def handle_update(%Event{action: :update, old: old, new: new}) do
    handle_hidden(new, User.hidden?(old), User.hidden?(new))
  end

  # Newly hidden
  defp handle_hidden(user, false, true) do
    GeoFence.exit_all_bots(user, "hide")
  end

  defp handle_hidden(_, _, _), do: :ok
end
