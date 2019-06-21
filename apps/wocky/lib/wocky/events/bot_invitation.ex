defmodule Wocky.Events.BotInvitation do
  @moduledoc "New invitation sent to the notified user"

  alias Wocky.Account.User
  alias Wocky.Bots.Bot
  alias Wocky.Relation.Invitation

  defstruct [
    :to,
    :from,
    :bot,
    :invitation
  ]

  @type t :: %__MODULE__{
          to: User.t(),
          from: User.t(),
          bot: Bot.t(),
          invitation: Invitation.t()
        }

  use ExConstructor
end

defimpl Wocky.Notifier.Push.Event, for: Wocky.Events.BotInvitation do
  import Wocky.Notifier.Push.Utils

  def notify?(_), do: true

  def recipient(%{to: to}), do: to

  def message(%{from: from, bot: bot}) do
    get_handle(from) <> " invited you to follow " <> get_title(bot)
  end

  def uri(%{bot: bot}), do: make_uri(:bot, bot.id)

  def opts(_), do: []
end

defimpl Wocky.Notifier.InBand.Event, for: Wocky.Events.BotInvitation do
  def notify?(_), do: true

  def event_type(_), do: :bot_invitation

  def required_fields(_),
    do: [
      :bot_id,
      :bot_invitation_id,
      :other_user_id,
      :user_id
    ]

  def transform(event),
    do: %{
      bot_id: event.bot.id,
      bot_invitation_id: event.invitation.id,
      other_user_id: event.from.id,
      user_id: event.to.id
    }
end
