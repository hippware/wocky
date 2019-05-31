defmodule Wocky.Block do
  @moduledoc """
  DB interface module for blocks
  """

  use Elixometer
  use Wocky.Repo.Schema

  import Ecto.Query

  alias Ecto.Queryable
  alias Wocky.Account.User
  alias Wocky.Repo
  alias Wocky.Roster

  @foreign_key_type :binary_id
  @primary_key false
  schema "blocks" do
    field :blocker_id, :binary_id, primary_key: true
    field :blockee_id, :binary_id, primary_key: true

    timestamps()

    belongs_to :blocker, User, define_field: false
    belongs_to :blockee, User, define_field: false
  end

  @type t :: %Block{}

  @doc "Blocker initiates a block on blockee"
  @spec block(User.t(), User.t()) :: :ok
  def block(blocker, blockee) do
    %Block{
      blocker_id: blocker.id,
      blockee_id: blockee.id
    }
    |> Repo.insert(on_conflict: :nothing)

    Roster.unfriend(blocker, blockee)

    update_counter("blocking.blocked", 1)

    :ok
  end

  @spec unblock(User.t(), User.t()) :: :ok
  def unblock(blocker, blockee) do
    Block
    |> where(blocker_id: ^blocker.id, blockee_id: ^blockee.id)
    |> Repo.delete_all()

    update_counter("blocking.unblocked", 1)

    :ok
  end

  @spec blocked?(User.t(), User.t()) :: boolean
  def blocked?(%User{id: id1}, %User{id: id2}), do: blocked?(id1, id2)

  @spec blocked?(User.id(), User.id()) :: boolean
  def blocked?(u1_id, u2_id) do
    Block
    |> where(
      [b],
      (b.blocker_id == ^u1_id and b.blockee_id == ^u2_id) or
        (b.blocker_id == ^u2_id and b.blockee_id == ^u1_id)
    )
    |> Repo.all() != []
  end

  @spec blocks_query(User.id()) :: Queryable.t()
  def blocks_query(user_id) do
    Block
    |> where(blocker_id: ^user_id)
  end

  @doc """
  Composable query fragment to filter out objects with owners that are blocking/
  blocked by the supplied user.
  """
  @spec object_visible_query(Queryable.t(), User.t(), atom) :: Queryable.t()
  def object_visible_query(query, requester, owner_field \\ :user_id) do
    query
    |> join(
      :left,
      [..., o],
      b in Block,
      on:
        (field(o, ^owner_field) == b.blocker_id and
           b.blockee_id == ^requester.id) or
          (field(o, ^owner_field) == b.blockee_id and
             b.blocker_id == ^requester.id)
    )
    |> where([..., b], is_nil(b.blocker_id))
  end
end
