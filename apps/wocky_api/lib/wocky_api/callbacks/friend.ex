defmodule WockyAPI.Callbacks.Friend do
  @moduledoc """
  Callbacks for roster item changes
  """

  use DawdleDB.Handler, type: Wocky.Friends.Friend

  alias Wocky.Repo.Hydrator
  alias WockyAPI.Resolvers.Friend

  def handle_insert(new),
    do: send_update(new, :friend)

  def handle_delete(old),
    do: send_update(old, :none)

  defp send_update(item, relationship) do
    Hydrator.with_assocs(item, [:contact], fn rec ->
      Friend.notify_contact(rec, relationship)
    end)
  end
end
