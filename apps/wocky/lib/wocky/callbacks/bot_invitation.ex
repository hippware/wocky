defmodule Wocky.Callbacks.BotInvitation do
  @moduledoc """
  DB Callback handler for bot invitations
  """

  use DawdleDB.Handler, type: Wocky.Bot.Invitation

  alias Wocky.Bot.Invitation
  alias Wocky.Events.{BotInvitation, BotInvitationResponse}
  alias Wocky.Notifier
  alias Wocky.Repo

  def handle_insert(new) do
    new = Repo.preload(new, [:user, :invitee, :bot])

    if new.user != nil && new.invitee != nil && new.bot != nil do
      %BotInvitation{
        to: new.invitee,
        from: new.user,
        bot: new.bot,
        invitation: new
      }
      |> Notifier.notify()
    end
  end

  def handle_update(
        %Invitation{accepted: accepted?} = new,
        %Invitation{accepted: nil}
      )
      when not is_nil(accepted?) do
    new = Repo.preload(new, [:user, :invitee, :bot])

    if new.user != nil && new.invitee != nil && new.bot != nil do
      %BotInvitationResponse{
        to: new.user,
        from: new.invitee,
        bot: new.bot,
        invitation: new
      }
      |> Notifier.notify()
    end
  end

  def handle_update(_new, _old), do: :ok
end
