defmodule Wocky.Events.NearbyStart do
  @moduledoc "Notification for a user being within range for nearby sharing"

  alias Wocky.Account.User

  defstruct [
    :to,
    :from,
    :last_push,
    :push_cooldown,
    :previously_nearby
  ]

  @type t :: %__MODULE__{
          to: User.t(),
          from: User.t(),
          last_push: DateTime.t(),
          push_cooldown: non_neg_integer(),
          previously_nearby: boolean()
        }

  use ExConstructor
end

defimpl Wocky.Notifier.Push.Event, for: Wocky.Events.NearbyStart do
  import Wocky.Notifier.Push.Utils

  alias Wocky.Contacts

  @impl true
  def notify?(notification) do
    notify? =
      (notification.last_push || DateTime.from_unix!(0))
      |> DateTime.add(notification.push_cooldown, :millisecond)
      |> DateTime.compare(DateTime.utc_now())
      |> Kernel.==(:lt)

    _ =
      if notify? do
        {:ok, _} =
          Contacts.update_last_start_notification(
            notification.from,
            notification.to
          )
      end

    notify?
  end

  @impl true
  def recipient(%{to: to}), do: to

  @impl true
  def message(%{from: from} = _event) do
    get_handle(from) <> " is nearby and sharing their location with you."
  end

  @impl true
  def uri(_event), do: nil

  @impl true
  def ignore_block?(_event), do: false

  @impl true
  def opts(_), do: []
end

defimpl Wocky.Notifier.InBand.Event, for: Wocky.Events.NearbyStart do
  @impl true
  def notify?(%{previously_nearby: previously_nearby}), do: !previously_nearby

  @impl true
  def event_type(_), do: :location_share_nearby_start

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
