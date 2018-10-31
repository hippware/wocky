defmodule Wocky.Callbacks.BotInvitation do
  @moduledoc """
  DB Callback handler for bot invitations
  """

  alias Wocky.Bot.Invitation
  alias Wocky.Push
  alias Wocky.Push.Events.{BotInvitationAcceptEvent, BotInviteEvent}
  alias Wocky.Repo
  alias Wocky.User.Notification.Invitation, as: InvNotification
  alias Wocky.User.Notification.InvitationResponse

  use Wocky.Watcher, type: Wocky.Bot.Invitation, events: [:insert, :update]

  def handle_insert(%Event{new: new}) do
    new = Repo.preload(new, [:user, :invitee, :bot])

    if new.user != nil && new.invitee != nil && new.bot != nil do
      InvNotification.notify(new)

      event =
        BotInviteEvent.new(%{from: new.user, to: new.invitee, bot: new.bot})

      Push.notify_all(new.invitee.id, event)
    end
  end

  def handle_update(%Event{
        old: %Invitation{accepted: nil},
        new: %Invitation{accepted: accepted?} = new
      })
      when not is_nil(accepted?) do
    new = Repo.preload(new, [:user, :invitee, :bot])

    if new.user != nil && new.invitee != nil && new.bot != nil do
      InvitationResponse.notify(new)

      if accepted? do
        event =
          BotInvitationAcceptEvent.new(%{
            from: new.invitee,
            to: new.user,
            bot: new.bot
          })

        Push.notify_all(new.user.id, event)
      end
    end
  end

  def handle_update(_), do: :ok
end
