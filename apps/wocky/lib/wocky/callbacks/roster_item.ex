defmodule Wocky.Callbacks.RosterItem do
  @moduledoc """
  Callbacks for roster item changes
  """

  use DawdleDB.Handler, type: Wocky.Roster.Item

  alias Wocky.Bot.Invitation
  alias Wocky.Bot.Subscription
  alias Wocky.Location
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

      Subscription.delete_for_owned_bots(contact, user)
      Subscription.delete_for_owned_bots(user, contact)

      Invitation.delete(contact, user)
      Invitation.delete(user, contact)
    end)

    :ok
  end
end
