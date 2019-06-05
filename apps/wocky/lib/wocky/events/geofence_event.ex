defmodule Wocky.Events.GeofenceEvent do
  @moduledoc """
  Notification for a guest entering or exiting a bot to which the notified
  user is subscribed
  """

  alias Wocky.Account.User
  alias Wocky.Bot

  defstruct [
    :to,
    :from,
    :bot,
    :event
  ]

  @type t :: %__MODULE__{
          to: User.t(),
          from: User.t(),
          bot: Bot.t(),
          event: :enter | :exit
        }

  use ExConstructor
end

defimpl Wocky.Notifier.Push.Event, for: Wocky.Events.GeofenceEvent do
  import Wocky.Notifier.Push.Utils

  def notify?(_), do: true

  def recipient(%{to: to}), do: to

  def message(%{from: from, bot: bot, event: event}) do
    case event do
      :enter -> "#{get_handle(from)} is at #{get_title(bot)}"
      :exit -> "#{get_handle(from)} left #{get_title(bot)}"
    end
  end

  def uri(%{bot: bot}), do: make_uri(:bot, bot.id, true, "visitors")

  def opts(_), do: []
end

defimpl Wocky.Notifier.InBand.Event, for: Wocky.Events.GeofenceEvent do
  def notify?(_), do: true

  def event_type(_), do: :geofence_event

  def required_fields(_),
    do: [
      :bot_id,
      :geofence_event,
      :other_user_id,
      :user_id
    ]

  def transform(event),
    do: %{
      bot_id: event.bot.id,
      geofence_event: event.event,
      other_user_id: event.from.id,
      user_id: event.to.id
    }
end
