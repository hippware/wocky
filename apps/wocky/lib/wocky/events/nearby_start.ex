defmodule Wocky.Events.NearbyStart do
  @moduledoc "Notification for a user being within range for nearby sharing"

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

defimpl Wocky.Notifier.Push.Event, for: Wocky.Events.NearbyStart do
  import Wocky.Notifier.Push.Utils

  def notify?(_), do: true

  def recipient(%{to: to}), do: to

  def message(%{from: from} = _event) do
    get_handle(from) <> " is nearby and sharing their location with you."
  end

  def uri(_event), do: nil

  def ignore_block?(_event), do: false

  def opts(_), do: []
end

defimpl Wocky.Notifier.InBand.Event, for: Wocky.Events.NearbyStart do
  def notify?(_), do: true

  def event_type(_), do: :location_share_nearby_start

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

  def ignore_block?(_event), do: false
end
