defmodule Wocky.Callbacks.RosterItem do
  @moduledoc """
  Callbacks for roster item changes
  """

  use DawdleDB.Handler, type: Wocky.Roster.Item

  alias Wocky.Bot.{Invitation, Subscription}
  alias Wocky.{Location, Repo}
  alias Wocky.Roster.Item

  def handle_delete(old) do
    %Item{user: user, contact: contact} = Repo.preload(old, [:user, :contact])

    # Cancel location sharing
    if user != nil and contact != nil do
      Location.stop_sharing_location(user, contact)
      Location.stop_sharing_location(contact, user)

      Subscription.delete_for_owned_bots(contact, user)
      Subscription.delete_for_owned_bots(user, contact)

      Invitation.delete(contact, user)
      Invitation.delete(user, contact)
    end

    :ok
  end
end
