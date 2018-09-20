defmodule Wocky.User.Notification.Invitation do
  @moduledoc "New invitation sent to the notified user"

  alias Wocky.User.Notification

  defstruct [
    :invitation_id,
    :user_id,
    :other_user_id,
    :bot_id
  ]

  @type t :: %__MODULE__{}

  def notify(invitation) do
    Notification.notify(%__MODULE__{
      invitation_id: invitation.id,
      user_id: invitation.invitee_id,
      other_user_id: invitation.user_id,
      bot_id: invitation.bot_id
    })
  end
end

defimpl Wocky.User.Notifier, for: Wocky.User.Notification.Invitation do
  alias Wocky.User.Notification

  def notify(invitation) do
    Notification.put(
      invitation,
      :invitation,
      [:invitation_id, :user_id, :other_user_id, :bot_id]
    )
  end

  def decode(invitation, params) do
    %{
      invitation
      | invitation_id: params.invitation_id,
        user_id: params.user_id,
        other_user_id: params.other_user_id,
        bot_id: params.bot_id
    }
  end
end
