defmodule Wocky.Collections.Collection do
  @moduledoc "Represents a collection of bots"

  use Wocky.Repo.Schema

  alias Wocky.Bot
  alias Wocky.Collections.{Member, Subscription}
  alias Wocky.User

  @foreign_key_type :binary_id
  schema "collections" do
    field :title, :string

    timestamps()

    belongs_to :user, User
    many_to_many :members, Bot, join_through: Member, unique: true
    many_to_many :subscribers, User, join_through: Subscription, unique: true
  end

  @type t :: %Collection{}
  @type id :: integer()

  @doc false
  def changeset(struct, params) do
    struct
    |> cast(params, [:title])
  end

  @doc false
  def bots_query(collection) do
    Ecto.assoc(collection, :members)
  end

  @doc false
  def subscribers_query(collection) do
    Ecto.assoc(collection, :subscribers)
  end
end
