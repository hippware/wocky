defmodule Wocky.User.Notification.InvitationResponse do
  @moduledoc "Notification for response to the notified user's invitation"

  alias Wocky.User.Notification

  defstruct [
    :invitation_id,
    :user_id,
    :other_user_id,
    :bot_id,
    :accepted
  ]

  @type t :: %__MODULE__{}

  def notify(invitation) do
    Notification.notify(%__MODULE__{
      invitation_id: invitation.id,
      user_id: invitation.user_id,
      other_user_id: invitation.invitee_id,
      bot_id: invitation.bot_id,
      accepted: invitation.accepted
    })
  end
end

defimpl Wocky.User.Notifier, for: Wocky.User.Notification.InvitationResponse do
  alias Wocky.User.Notification

  def notify(invitation_response) do
    invitation_response
    |> Map.put(:invitation_accepted, invitation_response.accepted)
    |> Notification.put(
      :invitation_response,
      [:invitation_id, :user_id, :other_user_id, :bot_id, :invitation_accepted]
    )
  end

  def decode(invitation_response, params) do
    %{
      invitation_response
      | user_id: params.user_id,
        other_user_id: params.other_user_id,
        bot_id: params.bot_id,
        invitation_id: params.invitation_id,
        accepted: params.invitation_accepted
    }
  end
end
