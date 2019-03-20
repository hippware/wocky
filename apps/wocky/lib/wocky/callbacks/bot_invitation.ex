defmodule Wocky.Callbacks.BotInvitation do
  @moduledoc """
  DB Callback handler for bot invitations
  """

  use DawdleDB.Handler, type: Wocky.Bot.Invitation

  alias Wocky.Bot.Invitation
  alias Wocky.Push
  alias Wocky.Push.Events.{BotInvitationAcceptEvent, BotInviteEvent}
  alias Wocky.Repo
  alias Wocky.User.Notification.BotInvitation, as: InvNotification
  alias Wocky.User.Notification.BotInvitationResponse

  def handle_insert(new) do
    new = Repo.preload(new, [:user, :invitee, :bot])

    if new.user != nil && new.invitee != nil && new.bot != nil do
      InvNotification.notify(new)

      event =
        BotInviteEvent.new(%{from: new.user, to: new.invitee, bot: new.bot})

      Push.notify_all(new.invitee, event)
    end
  end

  def handle_update(
        %Invitation{accepted: accepted?} = new,
        %Invitation{accepted: nil}
      )
      when not is_nil(accepted?) do
    new = Repo.preload(new, [:user, :invitee, :bot])

    if new.user != nil && new.invitee != nil && new.bot != nil do
      BotInvitationResponse.notify(new)

      if accepted? do
        event =
          BotInvitationAcceptEvent.new(%{
            from: new.invitee,
            to: new.user,
            bot: new.bot
          })

        Push.notify_all(new.user, event)
      end
    end
  end

  def handle_update(_new, _old), do: :ok
end
