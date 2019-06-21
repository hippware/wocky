defmodule Wocky.Callbacks.RosterItem do
  @moduledoc """
  Callbacks for roster item changes
  """

  use DawdleDB.Handler, type: Wocky.Roster.Item

  alias Wocky.Location
  alias Wocky.Relation
  alias Wocky.Repo.Hydrator
  alias Wocky.Roster.Item

  def handle_delete(old) do
    # Cancel location sharing
    Hydrator.with_assocs(old, [:user, :contact], fn %Item{
                                                      user: user,
                                                      contact: contact
                                                    } ->
      Location.stop_sharing_location(user, contact)
      Location.stop_sharing_location(contact, user)

      Relation.delete_subscriptions_for_owned_bots(contact, user)
      Relation.delete_subscriptions_for_owned_bots(user, contact)

      Relation.delete_invitation(contact, user)
      Relation.delete_invitation(user, contact)
    end)

    :ok
  end
end
