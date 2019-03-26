defmodule WockyAPI.Callbacks.RosterItem do
  @moduledoc """
  Callbacks for roster item changes
  """

  use DawdleDB.Handler, type: Wocky.Roster.Item

  alias Wocky.Repo
  alias WockyAPI.Resolvers.User

  def handle_insert(new),
    do: send_update(new, :friend)

  def handle_delete(old),
    do: send_update(old, :none)

  defp send_update(item, relationship) do
    item = Repo.preload(item, [:contact])

    if item.contact != nil do
      User.notify_contact(item, relationship)
    end
  end
end
