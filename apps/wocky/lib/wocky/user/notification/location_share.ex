defmodule Wocky.User.Notification.LocationShare do
  @moduledoc "A user has started to share their location to the recipient"

  alias Wocky.User.Notification

  defstruct [
    :user_id,
    :other_user_id
  ]

  @type t :: %__MODULE__{}

  def notify(share) do
    Notification.notify(%__MODULE__{
      user_id: share.shared_with_id,
      other_user_id: share.user_id
    })
  end
end

defimpl Wocky.User.Notifier, for: Wocky.User.Notification.LocationShare do
  alias Wocky.User.Notification

  def notify(invitation) do
    Notification.put(
      invitation,
      :location_share,
      [:user_id, :other_user_id]
    )
  end

  def decode(invitation, params) do
    %{
      invitation
      | user_id: params.user_id,
        other_user_id: params.other_user_id
    }
  end
end
