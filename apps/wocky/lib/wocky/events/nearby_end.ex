# credo:disable-for-this-file Credo.Check.Readability.StrictModuleLayout
defmodule Wocky.Events.NearbyEnd do
  @moduledoc "Notification for a user exiting range for nearby sharing"

  alias Wocky.Account.User

  defstruct [
    :to,
    :from,
    :previously_nearby
  ]

  @type t :: %__MODULE__{
          to: User.t(),
          from: User.t(),
          previously_nearby: boolean()
        }

  use ExConstructor
end

defimpl Wocky.Notifier.InBand.Event, for: Wocky.Events.NearbyEnd do
  @impl true
  def notify?(%{previously_nearby: previously_nearby}), do: previously_nearby

  @impl true
  def event_type(_), do: :location_share_nearby_end

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
