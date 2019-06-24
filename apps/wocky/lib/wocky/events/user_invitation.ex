defmodule Wocky.Events.UserInvitation do
  @moduledoc "Notification for a user inviting another user to be their friend"

  alias Wocky.Account.User

  defstruct [
    :to,
    :from
  ]

  @type t :: %__MODULE__{
          to: User.t(),
          from: User.t()
        }

  use ExConstructor
end

defimpl Wocky.Notifier.Push.Event, for: Wocky.Events.UserInvitation do
  import Wocky.Notifier.Push.Utils

  def notify?(_), do: true

  def recipient(%{to: to}), do: to

  def message(%{from: from} = _event) do
    get_handle(from) <> " wants to connect with you."
  end

  def uri(%{from: from} = _event), do: make_uri(:invitations, from.id)

  def opts(_), do: []
end

defimpl Wocky.Notifier.InBand.Event, for: Wocky.Events.UserInvitation do
  def notify?(_), do: true

  def event_type(_), do: :user_invitation

  def required_fields(_),
    do: [
      :other_user_id,
      :user_id
    ]

  def transform(event),
    do: %{
      other_user_id: event.from.id,
      user_id: event.to.id
    }
end
