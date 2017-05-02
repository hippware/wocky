defmodule Wocky.Bot.Item do
  @moduledoc "Represents an item published to a bot"

  use Wocky.Repo.Model

  alias Wocky.Bot
  alias __MODULE__, as: Item

  @foreign_key_type :binary_id
  @primary_key {:id, :string, autogenerate: false}
  schema "bot_items" do
    field :stanza, :string
    field :image,  :boolean, default: false

    timestamps()

    belongs_to :bot, Bot
  end

  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [:id, :bot_id, :stanza, :image])
    |> validate_required([:id, :bot_id, :stanza])
    |> foreign_key_constraint(:bot_id)
  end

  def put(params) do
    %Item{}
    |> changeset(params)
    |> Repo.insert(on_conflict: :replace_all, conflict_target: [:id, :bot_id])
  end
end
