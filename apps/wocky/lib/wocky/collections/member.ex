defmodule Wocky.Collections.Member do
  @moduledoc "Represents a member of collection of bots"

  use Wocky.Repo.Schema

  alias Wocky.Bot
  alias Wocky.Collections.Collection

  @primary_key false
  schema "collection_members" do
    field :collection_id, :id, primary_key: true
    field :bot_id, :binary_id, primary_key: true

    timestamps()

    belongs_to :collection, Collection, define_field: false
    belongs_to :bot, Bot, type: :binary_id, define_field: false
  end

  @type t :: %Member{}

  @doc false
  def changeset(struct, params) do
    struct
    |> cast(params, [:collection_id, :bot_id])
    |> validate_required([:collection_id, :bot_id])
    |> foreign_key_constraint(:collection_id)
    |> foreign_key_constraint(:bot_id)
  end
end
