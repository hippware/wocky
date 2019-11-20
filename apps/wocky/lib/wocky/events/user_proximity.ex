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

  @impl true
  def notify?(_), do: true

  @impl true
  def recipient(%{to: to}), do: to

  @impl true
  def message(%{from: from} = _event) do
    get_handle(from) <> " is within range - say hi."
  end

  @impl true
  def uri(_event), do: nil

  @impl true
  def ignore_block?(_event), do: false

  @impl true
  def opts(_), do: []
end

defimpl Wocky.Notifier.InBand.Event, for: Wocky.Events.UserProximity do
  @impl true
  def notify?(_), do: true

  @impl true
  def event_type(_), do: :user_proximity

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
