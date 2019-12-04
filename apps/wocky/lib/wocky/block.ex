defmodule Wocky.Block do
  @moduledoc """
  DB interface module for blocks
  """

  use Elixometer
  use Wocky.Repo.Schema

  import Ecto.Query

  alias Ecto.Queryable
  alias Wocky.Account.User
  alias Wocky.Friends
  alias Wocky.Repo

  @foreign_key_type :binary_id
  @primary_key false
  schema "blocks" do
    field :blocker_id, :binary_id, primary_key: true
    field :blockee_id, :binary_id, primary_key: true

    timestamps()

    belongs_to :blocker, User, define_field: false
    belongs_to :blockee, User, define_field: false
  end

  @type t :: %__MODULE__{}

  @doc "Blocker initiates a block on blockee"
  @spec block(User.tid(), User.tid()) :: :ok
  def block(blocker, blockee) do
    Repo.insert(
      %__MODULE__{
        blocker_id: User.id(blocker),
        blockee_id: User.id(blockee)
      },
      on_conflict: :nothing
    )

    Friends.unfriend(blocker, blockee)

    update_counter("blocking.blocked", 1)

    :ok
  end

  @spec unblock(User.tid(), User.tid()) :: :ok
  def unblock(blocker, blockee) do
    __MODULE__
    |> where(blocker_id: ^User.id(blocker), blockee_id: ^User.id(blockee))
    |> Repo.delete_all()

    update_counter("blocking.unblocked", 1)

    :ok
  end

  @spec blocked?(User.tid(), User.tid()) :: boolean()
  def blocked?(u1, u2) do
    u1_id = User.id(u1)
    u2_id = User.id(u2)

    __MODULE__
    |> where(
      [b],
      (b.blocker_id == ^u1_id and b.blockee_id == ^u2_id) or
        (b.blocker_id == ^u2_id and b.blockee_id == ^u1_id)
    )
    |> Repo.exists?()
  end

  @spec blocks_query(User.tid()) :: Queryable.t()
  def blocks_query(user) do
    where(__MODULE__, blocker_id: ^User.id(user))
  end

  @doc """
  Composable query fragment to filter out objects with owners that are blocking/
  blocked by the supplied user.
  """
  @spec object_visible_query(Queryable.t(), User.tid(), atom()) :: Queryable.t()
  def object_visible_query(query, requester, owner_field \\ :user_id) do
    query
    |> join(
      :left,
      [..., o],
      b in __MODULE__,
      on:
        (field(o, ^owner_field) == b.blocker_id and
           b.blockee_id == ^User.id(requester)) or
          (field(o, ^owner_field) == b.blockee_id and
             b.blocker_id == ^User.id(requester))
    )
    |> where([..., b], is_nil(b.blocker_id))
  end
end
