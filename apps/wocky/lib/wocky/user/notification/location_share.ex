defmodule Wocky.User.Notification.LocationShare do
  @moduledoc "A user has started to share their location to the recipient"

  alias Wocky.User.Notification

  defstruct [
    :user_id,
    :other_user_id,
    :expires_at
  ]

  @type t :: %__MODULE__{}

  def notify(share) do
    Notification.notify(%__MODULE__{
      user_id: share.shared_with_id,
      other_user_id: share.user_id,
      expires_at: share.expires_at
    })
  end
end

defimpl Wocky.User.Notifier, for: Wocky.User.Notification.LocationShare do
  alias Wocky.User.Notification

  def notify(share) do
    Notification.put(
      share,
      :location_share,
      [:user_id, :other_user_id, :expires_at]
    )
  end

  def decode(share, params) do
    %{
      share
      | user_id: params.user_id,
        other_user_id: params.other_user_id,
        expires_at: params.expires_at
    }
  end
end
