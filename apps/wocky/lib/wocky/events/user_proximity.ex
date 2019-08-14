defmodule Wocky.Events.UserProximity do
  @moduledoc "Notification for a user being within a specified range of another"

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

defimpl Wocky.Notifier.Push.Event, for: Wocky.Events.UserProximity do
  import Wocky.Notifier.Push.Utils

  def notify?(_), do: true

  def recipient(%{to: to}), do: to

  def message(%{from: from} = _event) do
    get_handle(from) <> " is within range - say hi."
  end

  def uri(_event), do: nil

  def opts(_), do: []
end

defimpl Wocky.Notifier.InBand.Event, for: Wocky.Events.UserProximity do
  def notify?(_), do: true

  def event_type(_), do: :user_proximity

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
