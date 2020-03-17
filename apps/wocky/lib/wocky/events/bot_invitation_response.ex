# credo:disable-for-this-file Credo.Check.Readability.StrictModuleLayout
defmodule Wocky.Events.BotInvitationResponse do
  @moduledoc "Event for response to the notified user's invitation"

  alias Wocky.Account.User
  alias Wocky.POI.Bot
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

defimpl Wocky.Notifier.Push.Event, for: Wocky.Events.BotInvitationResponse do
  import Wocky.Notifier.Push.Utils

  @impl true
  def notify?(%{invitation: %{accepted: accepted}}), do: accepted

  @impl true
  def recipient(%{to: to}), do: to

  @impl true
  def message(%{from: from, bot: bot}) do
    get_handle(from) <> " accepted your invitation to " <> get_title(bot)
  end

  @impl true
  def uri(%{bot: bot}), do: make_uri(:bot, bot.id)

  @impl true
  def ignore_block?(_event), do: false

  @impl true
  def opts(_), do: []
end

defimpl Wocky.Notifier.InBand.Event, for: Wocky.Events.BotInvitationResponse do
  @impl true
  def notify?(_), do: true

  @impl true
  def event_type(_), do: :bot_invitation_response

  @impl true
  def required_fields(_),
    do: [
      :bot_id,
      :bot_invitation_accepted,
      :bot_invitation_id,
      :other_user_id,
      :user_id
    ]

  @impl true
  def transform(event),
    do: %{
      bot_id: event.bot.id,
      bot_invitation_accepted: event.invitation.accepted,
      bot_invitation_id: event.invitation.id,
      other_user_id: event.from.id,
      user_id: event.to.id
    }

  @impl true
  def ignore_block?(_event), do: false
end
