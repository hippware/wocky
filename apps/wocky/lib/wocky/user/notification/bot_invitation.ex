defmodule Wocky.User.Notification.BotInvitation do
  @moduledoc "New invitation sent to the notified user"

  alias Wocky.User.Notification

  defstruct [
    :bot_invitation_id,
    :user_id,
    :other_user_id,
    :bot_id
  ]

  @type t :: %__MODULE__{}

  def notify(invitation) do
    Notification.notify(%__MODULE__{
      bot_invitation_id: invitation.id,
      user_id: invitation.invitee_id,
      other_user_id: invitation.user_id,
      bot_id: invitation.bot_id
    })
  end
end

defimpl Wocky.User.Notifier, for: Wocky.User.Notification.BotInvitation do
  alias Wocky.User.Notification

  def notify(invitation) do
    Notification.put(
      invitation,
      :bot_invitation,
      [:bot_invitation_id, :user_id, :other_user_id, :bot_id]
    )
  end

  def decode(invitation, params) do
    %{
      invitation
      | bot_invitation_id: params.bot_invitation_id,
        user_id: params.user_id,
        other_user_id: params.other_user_id,
        bot_id: params.bot_id
    }
  end
end
