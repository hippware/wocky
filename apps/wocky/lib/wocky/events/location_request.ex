defmodule Wocky.Events.LocationRequest do
  @moduledoc """
  Notification to requeset a user's device to send its current location
  """

  alias Wocky.Account.User

  defstruct [
    :to
  ]

  @type t :: %__MODULE__{
          to: User.t()
        }

  use ExConstructor
end

defimpl Wocky.Notifier.Push.Event, for: Wocky.Events.LocationRequest do
  @impl true
  def notify?(_), do: true

  @impl true
  def recipient(%{to: to}), do: to

  @impl true
  def message(_), do: ""

  @impl true
  def uri(_), do: ""

  @impl true
  def opts(_) do
    [
      priority: :high,
      background: true,
      extra_fields: %{"location-request" => 1}
    ]
  end

  @impl true
  def ignore_block?(_event), do: false
end
