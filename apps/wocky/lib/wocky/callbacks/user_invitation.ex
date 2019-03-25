defmodule Wocky.Callbacks.UserInvitation do
  @moduledoc """
  DB Callback handler for user invitations
  """

  use DawdleDB.Handler, type: Wocky.Roster.Invitation

  alias Wocky.Push
  alias Wocky.Push.Events.UserInvitationEvent
  alias Wocky.Repo
  alias Wocky.User.Notification.UserInvitation, as: InvNotification

  def handle_insert(new) do
    new = Repo.preload(new, [:user, :invitee])

    if new.user != nil && new.invitee != nil do
      {:ok, _} = InvNotification.notify(new.user, new.invitee)

      event = UserInvitationEvent.new(%{from: new.user, to: new.invitee})

      Push.notify_all(new.invitee, event)
    end
  end
end
