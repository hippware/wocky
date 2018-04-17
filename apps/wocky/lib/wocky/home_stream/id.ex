defmodule Wocky.HomeStream.ID do
  use Wocky.JID

  alias Wocky.Bot
  alias Wocky.Bot.Item
  alias Wocky.Repo.ID
  alias Wocky.User

  @moduledoc """
  Common helper functions for home stream functionality
  """

  ### ID Generator functions for different item classes
  @type t :: {binary, User.t() | nil, Bot.t() | nil, Item.t() | nil}

  # ID for bot show/share events
  @spec bot_id(Bot.t()) :: t()
  def bot_id(bot), do: {JID.to_binary(Bot.to_jid(bot)), nil, bot, nil}

  # ID for publication event
  @spec bot_event_id(Bot.t(), Item.t() | binary) :: t()
  def bot_event_id(bot, %Item{} = item), do: bot_event_id(bot, item, "event")

  def bot_event_id(bot, event) when is_binary(event) do
    bot_event_id(bot, nil, event)
  end

  # ID for user entry/exit. These are suffixed with a unique ID so that
  # they never overwrite each other
  @spec bot_event_id(Bot.t(), User.t() | Item.t() | nil, binary) :: t()
  def bot_event_id(bot, %User{} = user, event) do
    str =
      JID.to_binary(Bot.to_jid(bot)) <>
        "/" <>
        event <>
        "/" <> JID.to_binary(User.to_jid(user)) <> "/instance/" <> ID.new()

    {str, user, bot, nil}
  end

  # ID for follow on/off/expire/publish
  def bot_event_id(bot, item, event) do
    bot_jid = Bot.to_jid(bot)

    str =
      bot_jid
      |> JID.replace_resource(jid(bot_jid, :resource) <> "/" <> event)
      |> JID.to_binary()

    {str, nil, bot, item}
  end

  # ID for any other bot events for which we only want
  # one ocurrence on the HS per bot
  @spec bot_event_id(Bot.t()) :: t()
  def bot_event_id(bot), do: bot_event_id(bot, nil, "event")

  # ID for bot decription change
  @spec bot_description_id(Bot.t()) :: t()
  def bot_description_id(bot), do: bot_event_id(bot, nil, "description")

  # ID for a referenced bot update
  @spec bot_changed_id(Bot.t()) :: t()
  def bot_changed_id(bot), do: bot_event_id(bot, nil, "changed")

  # ID for chat message from user
  @spec user_message_id(User.t()) :: t()
  def user_message_id(user) do
    str =
      user
      |> User.to_jid()
      |> JID.to_bare()
      |> JID.to_binary()

    {str, user, nil, nil}
  end

  @spec collection_share_id(Collection.t()) :: t()
  def collection_share_id(collection),
  do: "collection/" <> to_string(collection.id) <> "/share"
end
