defmodule Wocky.User.Notification.UserFollow do
  @moduledoc "Notification for a new user following the notified user"

  alias Wocky.User.Notification

  defstruct [
    :user_id,
    :other_user_id
  ]

  @type t :: %__MODULE__{}

  def notify(follower, followee) do
    Notification.notify(%__MODULE__{
      user_id: followee.id,
      other_user_id: follower.id
    })
  end
end

defimpl Wocky.User.Notifier, for: Wocky.User.Notification.UserFollow do
  alias Wocky.User.Notification

  def notify(user_follow) do
    Notification.put(user_follow, :user_follow, [:user_id, :other_user_id])
  end

  def decode(user_follow, params) do
    %{
      user_follow
      | user_id: params.user_id,
        other_user_id: params.other_user_id
    }
  end
end
