defmodule Wocky.Callbacks.UserInvitation do
  @moduledoc """
  DB Callback handler for user invitations
  """

  use DawdleDB.Handler, type: Wocky.Roster.Invitation

  alias Wocky.Events.UserInvitation
  alias Wocky.Notifier
  alias Wocky.Repo.Hydrator

  def handle_insert(new) do
    Hydrator.with_assocs(new, [:user, :invitee], fn rec ->
      %UserInvitation{
        to: rec.invitee,
        from: rec.user
      }
      |> Notifier.notify()
    end)
  end
end
