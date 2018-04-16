defmodule Wocky.Collections.Subscription do
  @moduledoc "Represents a subscription to a collection of bots"

  use Wocky.Repo.Schema

  alias Wocky.Collections.Collection
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

  @doc false
  def changeset(struct, params) do
    struct
    |> cast(params, [:collection_id, :user_id])
    |> validate_required([:collection_id, :user_id])
    |> foreign_key_constraint(:collection_id)
    |> foreign_key_constraint(:user_id)
  end
end
