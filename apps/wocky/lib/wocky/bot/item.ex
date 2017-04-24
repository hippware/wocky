defmodule Wocky.Bot.Item do
  @moduledoc "Represents an item published to a bot"

  use Wocky.Repo.Model

  alias Wocky.Bot

  @foreign_key_type :binary_id
  @primary_key {:id, :string, autogenerate: false}
  schema "bot_items" do
    field :stanza, :string
    field :image,  :boolean, default: false

    timestamps()

    belongs_to :bot, Bot
  end
end
