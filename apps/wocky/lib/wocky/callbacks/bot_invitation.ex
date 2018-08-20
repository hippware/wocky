defmodule Wocky.Callbacks.BotInvitation do
  @moduledoc """
  DB Callback handler for bot invitations
  """

  alias Wocky.Bot.Invitation
  alias Wocky.User.Notification.Invitation, as: InvNotification
  alias Wocky.User.Notification.InvitationResponse

  use Wocky.Watcher, type: Wocky.Bot.Invitation, events: [:insert, :update]

  def handle_insert(%Event{new: new}) do
    InvNotification.notify(new)
  end

  def handle_update(%Event{
        old: %Invitation{accepted: nil},
        new: %Invitation{accepted: accepted?} = new
      })
      when not is_nil(accepted?) do
    InvitationResponse.notify(new)
  end

  def handle_update(_), do: :ok
end
