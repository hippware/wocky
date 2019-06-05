defmodule Wocky.Events.BotItem do
  @moduledoc """
  Notification for a newly posted or updated bot item on a bot
  to which the notified user is subscribed
  """

  alias Wocky.Account.User
  alias Wocky.Bot.Item

  defstruct [
    :to,
    :from,
    :item
  ]

  @type t :: %__MODULE__{
          to: User.t(),
          from: User.t(),
          item: Item.t()
        }

  use ExConstructor
end

defimpl Wocky.Notifier.Push.Event, for: Wocky.Events.BotItem do
  import Wocky.Notifier.Push.Utils

  def notify?(_), do: true

  def recipient(%{to: to}), do: to

  def message(%{from: from, item: item}) do
    get_handle(from) <> " commented on " <> get_title(item.bot)
  end

  def uri(%{item: item}), do: make_uri(:bot, item.bot.id)

  def opts(_), do: []
end

defimpl Wocky.Notifier.InBand.Event, for: Wocky.Events.BotItem do
  def notify?(_), do: true

  def event_type(_), do: :bot_item

  def required_fields(_),
    do: [
      :bot_id,
      :bot_item_id,
      :other_user_id,
      :user_id
    ]

  def transform(event),
    do: %{
      bot_id: event.item.bot.id,
      bot_item_id: event.item.id,
      other_user_id: event.from.id,
      user_id: event.to.id
    }
end
