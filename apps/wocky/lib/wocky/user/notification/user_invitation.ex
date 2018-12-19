defmodule Wocky.User.Notification.UserInvitation do
  @moduledoc "Notification for a user inviting another user to be their friend"

  alias Wocky.User.Notification

  defstruct [
    :user_id,
    :other_user_id
  ]

  @type t :: %__MODULE__{}

  def notify(user, invitee) do
    Notification.notify(%__MODULE__{
      user_id: invitee.id,
      other_user_id: user.id
    })
  end
end

defimpl Wocky.User.Notifier, for: Wocky.User.Notification.UserInvitation do
  alias Wocky.User.Notification

  def notify(user_follow) do
    Notification.put(user_follow, :user_invitation, [:user_id, :other_user_id])
  end

  def decode(user_follow, params) do
    %{
      user_follow
      | user_id: params.user_id,
        other_user_id: params.other_user_id
    }
  end
end
