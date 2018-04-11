defmodule Wocky.Collection.Member do
  @moduledoc "Represents a member of collection of bots"

  use Wocky.Repo.Schema

  import Ecto.Query

  alias Wocky.Bot
  alias Wocky.Collection
  alias Wocky.Repo

  @primary_key false
  schema "collection_members" do
    field :collection_id, :id, primary_key: true
    field :bot_id, :binary_id, primary_key: true

    timestamps()

    belongs_to :collection, Collection, define_field: false
    belongs_to :bot, Bot, type: :binary_id, define_field: false
  end

  @type t :: %Member{}

  def add(collection_id, bot_id) do
    %Member{}
    |> changeset(%{collection_id: collection_id, bot_id: bot_id})
    |> Repo.insert()
  end

  def remove(collection_id, bot_id) do
    Member
    |> where([m], m.collection_id == ^collection_id and m.bot_id == ^bot_id)
    |> Repo.delete_all()

    :ok
  end

  defp changeset(struct, params) do
    struct
    |> cast(params, [:collection_id, :bot_id])
    |> validate_required([:collection_id, :bot_id])
    |> foreign_key_constraint(:collection_id)
    |> foreign_key_constraint(:bot_id)
  end
end
