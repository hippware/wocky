defmodule Wocky.Events.LocationShare do
  @moduledoc "A user has started to share their location to the recipient"

  alias Wocky.Account.User

  defstruct [
    :to,
    :from,
    :expires_at
  ]

  @type t :: %__MODULE__{
          to: User.t(),
          from: User.t(),
          expires_at: DateTime.t()
        }
end

defimpl Wocky.Notifier.Push.Event, for: Wocky.Events.LocationShare do
  import Wocky.Notifier.Push.Utils

  def notify?(_), do: true

  def recipient(%{to: to}), do: to

  def message(%{from: from} = _event) do
    get_handle(from) <> " is sharing their location with you"
  end

  def uri(%{from: from} = _event), do: make_uri(:livelocation, from.id)

  def opts(_), do: []
end

defimpl Wocky.Notifier.InBand.Event, for: Wocky.Events.LocationShare do
  def notify?(_), do: true

  def event_type(_), do: :location_share

  def required_fields(_),
    do: [
      :expires_at,
      :other_user_id,
      :user_id
    ]

  def transform(event),
    do: %{
      expires_at: event.expires_at,
      other_user_id: event.from.id,
      user_id: event.to.id
    }
end
