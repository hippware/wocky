defmodule Wocky.Callbacks.UserInvitation do
  @moduledoc """
  DB Callback handler for user invitations
  """

  use DawdleDB.Handler, type: Wocky.Roster.Invitation

  alias Wocky.Events.UserInvitation
  alias Wocky.Notifier
  alias Wocky.Repo

  def handle_insert(new) do
    new = Repo.preload(new, [:user, :invitee])

    if new.user != nil && new.invitee != nil do
      %UserInvitation{
        to: new.invitee,
        from: new.user
      }
      |> Notifier.notify()
    end
  end
end
