defmodule Wocky.Callbacks.RosterItem do
  @moduledoc """
  Callbacks for roster item changes
  """

  use DawdleDB.Handler, type: Wocky.Roster.Item

  alias Wocky.Location
  alias Wocky.Relations
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

      Relations.delete_subscriptions_for_owned_bots(contact, user)
      Relations.delete_subscriptions_for_owned_bots(user, contact)

      Relations.delete_invitation(contact, user)
      Relations.delete_invitation(user, contact)
    end)

    :ok
  end
end
