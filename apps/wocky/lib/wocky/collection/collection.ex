defmodule Wocky.Collection do
  @moduledoc "Represents a collection of bots"

  use Wocky.Repo.Schema

  import Ecto.Query

  alias Ecto.Changeset
  alias Wocky.Collection.Member
  alias Wocky.Collection.Subscription
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
    |> cast(params, [:title, :description])
  end

  @spec create(binary(), User.t()) :: {:ok, t()}
  def create(title, user) do
    Repo.insert(%Collection{title: title, user_id: user.id}, returning: true)
  end

  @spec update(id(), binary()) :: {:ok, t()} | {:error, Changeset.t()}
  def update(id, title) do
    %Collection{id: id}
    |> changeset(%{title: title})
    |> Repo.insert()
  end

  @spec delete(id()) :: :ok
  def delete(id) do
    Collection
    |> where([c], c.id == ^id)
    |> Repo.delete_all()

    :ok
  end

  @spec add_bot(id(), Bot.id()) :: {:ok, Member.t()} | {:error, Changeset.t()}
  def add_bot(id, bot_id) do
    Member.add(id, bot_id)
  end

  @spec remove_bot(id(), Bot.id()) :: {:ok, Member.t()} | {:error, Changeset.t()}
  def remove_bot(id, bot_id) do
    Member.remove(id, bot_id)
  end

  @spec subscribe(id(), User.id()) :: {:ok, t()} | {:error, Changeset.t()}
  def subscribe(id, user_id) do
    Subscription.add(id, user_id)
  end

  @spec unsubscribe(id(), User.id()) :: {:ok, t()} | {:error, Changeset.t()}
  def unsubscribe(id, user_id) do
    Subscription.remove(id, user_id)
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
