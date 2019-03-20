defmodule Wocky.Callbacks.User do
  @moduledoc """
  Callbacks for DB user changes
  """

  use DawdleDB.Handler, type: Wocky.User

  alias Wocky.User
  alias Wocky.User.GeoFence

  # No change to hidden timeout - nothing to do
  def handle_update(new, old) do
    handle_hidden(new, User.hidden?(old), User.hidden?(new))
  end

  # Newly hidden
  defp handle_hidden(user, false, true) do
    GeoFence.exit_all_bots(user, "hide")
  end

  defp handle_hidden(_, _, _), do: :ok
end
