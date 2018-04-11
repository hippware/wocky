defmodule Wocky.Collection.Subscription do
  @moduledoc "Represents a subscription to a collection of bots"

  use Wocky.Repo.Schema

  import Ecto.Query

  alias Wocky.Collection
  alias Wocky.Collection.Subscription
  alias Wocky.Repo
  alias Wocky.User

  @primary_key false
  schema "collection_subscriptions" do
    field :collection_id, :id, primary_key: true
    field :user_id, :binary_id, primary_key: true

    timestamps()

    belongs_to :collection, Collection, define_field: false
    belongs_to :user, User, type: :binary_id, define_field: false
  end

  @type t :: %Subscription{}

  def add(collection_id, user_id) do
    %Subscription{}
    |> changeset(%{collection_id: collection_id, user_id: user_id})
    |> Repo.insert()
  end

  def remove(collection_id, user_id) do
    Subscription
    |> where([s], s.collection_id == ^collection_id and s.user_id == ^user_id)
    |> Repo.delete_all()

    :ok
  end

  defp changeset(struct, params) do
    struct
    |> cast(params, [:collection_id, :user_id])
    |> validate_required([:collection_id, :user_id])
    |> foreign_key_constraint(:collection_id)
    |> foreign_key_constraint(:user_id)
  end
end
