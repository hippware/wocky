defmodule Wocky.Collections.Collection do
  @moduledoc "Represents a collection of bots"

  use Wocky.Repo.Schema

  import Ecto.Query

  alias Ecto.Changeset
  alias Wocky.Collections.Member
  alias Wocky.Collections.Subscription
  alias Wocky.Bot
  alias Wocky.Repo
  alias Wocky.User

  @foreign_key_type :binary_id
  schema "collections" do
    field :title, :string

    timestamps()

    belongs_to :user, User
    many_to_many(:members, Bot, join_through: Member)
    many_to_many(:subscribers, User, join_through: Subscription)
  end

  @type t :: %Collection{}
  @type id :: integer()

  @spec changeset(t, map) :: Changeset.t()
  def changeset(struct, params) do
    struct
    |> cast(params, [:title])
  end

  @spec create(binary(), User.t()) :: {:ok, t()}
  def create(title, user) do
    Repo.insert(%Collection{title: title, user_id: user.id}, returning: true)
  end

  @spec update(id(), binary()) :: {:ok, t()} | {:error, Changeset.t()}
  def update(id, title) do
    %Collection{id: id}
    |> changeset(%{title: title})
    |> Repo.update()
  end

  @spec delete(id()) :: :ok
  def delete(id) do
    Collection
    |> where([c], c.id == ^id)
    |> Repo.delete_all()

    :ok
  end

  def owned_query(user_id) do
    Collection
    |> where([c], c.user_id == ^user_id)
  end

  def bots_query(collection) do
    Ecto.assoc(collection, :members)
  end

  def subscribers_query(collection) do
    Ecto.assoc(collection, :subscribers)
  end
end
