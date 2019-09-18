defmodule Wocky.Events.LocationShareEnd do
  @moduledoc "A user has stopped sharing their location to the recipient"

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

defimpl Wocky.Notifier.InBand.Event, for: Wocky.Events.LocationShareEnd do
  def notify?(_), do: true

  def event_type(_), do: :location_share_end

  def required_fields(_),
    do: [
      :other_user_id,
      :user_id,
      :share_id
    ]

  def transform(event),
    do: %{
      other_user_id: event.from.id,
      user_id: event.to.id,
      share_id: event.share_id
    }

  def ignore_block?(_event), do: true
end
