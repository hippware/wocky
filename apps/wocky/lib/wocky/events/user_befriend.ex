defmodule Wocky.Events.UserBefriend do
  @moduledoc """
  Notification for a user becoming friends with another user - sent to both the
  inviter and invitee upon completion of the friendship ritual.
  """

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

defimpl Wocky.Notifier.InBand.Event, for: Wocky.Events.UserBefriend do
  @impl true
  def notify?(_), do: true

  @impl true
  def event_type(_), do: :user_befriend

  @impl true
  def required_fields(_),
    do: [
      :other_user_id,
      :user_id
    ]

  @impl true
  def transform(event),
    do: %{
      other_user_id: event.from.id,
      user_id: event.to.id
    }

  @impl true
  def ignore_block?(_event), do: false
end
