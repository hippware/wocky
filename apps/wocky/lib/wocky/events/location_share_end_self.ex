defmodule Wocky.Events.LocationShareEndSelf do
  @moduledoc """
  The recipient has stopped sharing their location with the specified user
  """

  alias Wocky.Account.User

  defstruct [
    :to,
    :from,
    :share_id
  ]

  @type t :: %__MODULE__{
          to: User.t(),
          from: User.t(),
          share_id: non_neg_integer()
        }
end

defimpl Wocky.Notifier.InBand.Event, for: Wocky.Events.LocationShareEndSelf do
  @impl true
  def notify?(_), do: true

  @impl true
  def event_type(_), do: :location_share_end_self

  @impl true
  def required_fields(_),
    do: [
      :other_user_id,
      :user_id,
      :share_id
    ]

  @impl true
  def transform(event),
    do: %{
      other_user_id: event.from.id,
      user_id: event.to.id,
      share_id: event.share_id
    }

  @impl true
  def ignore_block?(_event), do: true
end
