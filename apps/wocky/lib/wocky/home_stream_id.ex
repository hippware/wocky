defmodule Wocky.HomeStreamID do
  use Wocky.JID

  alias Wocky.Bot
  alias Wocky.Repo.ID
  alias Wocky.User

  @moduledoc """
  Common helper functions for home stream functionality
  """

  ### ID Generator functions for different item classes
  @type id :: {binary, User.t | nil, Bot.t | nil}

  # ID for bot show/share events
  @spec bot_id(Bot.t) :: id
  def bot_id(bot), do: {JID.to_binary(Bot.to_jid(bot)), nil, bot}

  # ID for follow on/off/expire
  @spec bot_event_id(Bot.t, binary) :: id
  def bot_event_id(bot, event) do
    {JID.to_binary(Bot.to_jid(bot)) <> "/" <> event, nil, bot}
  end

  # ID for user entry/exit. These are suffixed with a unique ID so that
  # they never overwrite each other
  @spec bot_event_id(Bot.t, binary, User.t) :: id
  def bot_event_id(bot, event, user) do
    str =
      JID.to_binary(Bot.to_jid(bot))
      <> "/" <> event <> "/"
      <> JID.to_binary(User.to_jid(user))
      <> "/instance/" <> ID.new
    {str, user, bot}
  end

  # ID for publication event and any others for which we only want
  # one ocurrence on the HS per bot
  def bot_event_id(bot) do
    bot_jid = Bot.to_jid(bot)
    str =
      bot_jid
      |> JID.replace_resource(jid(bot_jid, :resource) <> "/event")
      |> JID.to_binary
    {str, nil, bot}
  end

  # ID for bot decription change
  @spec bot_description_id(Bot.t) :: id
  def bot_description_id(bot) do
    bot_jid = Bot.to_jid(bot)
    str =
      bot_jid
      |> JID.replace_resource(jid(bot_jid, :resource) <> "/description")
      |> JID.to_binary
    {str, nil, bot}
  end

  # ID for chat message from user
  @spec user_message_id(User.t) :: id
  def user_message_id(user) do
    str =
      user
      |> User.to_jid
      |> JID.to_bare
      |> JID.to_binary
    {str, user, nil}
  end
end
