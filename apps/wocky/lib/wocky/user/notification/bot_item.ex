defmodule Wocky.User.Notification.BotItem do
  @moduledoc """
  Notification for a newly posted or updated bot item on a bot
  to which the notified user is subscribed
  """

  alias Wocky.Bot
  alias Wocky.Push
  alias Wocky.Push.Events.NewBotItemEvent
  alias Wocky.Repo
  alias Wocky.User.Notification

  defstruct [
    :user_id,
    :other_user_id,
    :bot_id,
    :bot_item_id
  ]

  @type t :: __MODULE__

  def notify(%{bot: bot} = item) do
    %{user: user} = item = Repo.preload(item, :user)

    if user != nil do
      bot
      |> Bot.notification_recipients(user)
      |> Enum.each(&do_notify(&1, item))
    end
  end

  def do_notify(user, item) do
    %__MODULE__{
      user_id: user.id,
      other_user_id: item.user.id,
      bot_id: item.bot.id,
      bot_item_id: item.id
    }
    |> Notification.notify()

    event = NewBotItemEvent.new(author: item.user, to: user, item: item)
    Push.notify_all(user.id, event)
  end
end

defimpl Wocky.User.Notifier, for: Wocky.User.Notification.BotItem do
  alias Wocky.User.Notification

  def notify(bot_item) do
    Notification.put(
      bot_item,
      :bot_item,
      [:user_id, :other_user_id, :bot_id, :bot_item_id]
    )
  end

  def decode(bot_item, params) do
    %{
      bot_item
      | user_id: params.user_id,
        other_user_id: params.other_user_id,
        bot_id: params.bot_id,
        bot_item_id: params.bot_item_id
    }
  end
end
