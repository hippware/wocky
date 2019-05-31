defmodule Wocky.Roster.Item do
  @moduledoc """
  DB interface module for roster items
  """

  use Wocky.Repo.Schema

  import Ecto.Query

  alias Wocky.Account.User
  alias Wocky.Repo
  alias Wocky.Roster

  @foreign_key_type :binary_id
  schema "roster_items" do
    field :name, :binary, default: ""

    belongs_to :user, User
    belongs_to :contact, User

    timestamps()
  end

  @type name :: binary

  @type t :: %__MODULE__{
          user: User.t(),
          contact: User.t(),
          name: name,
          updated_at: DateTime.t()
        }

  @change_fields [:user_id, :contact_id, :name]

  @spec get(User.t(), User.t()) :: t() | nil
  def get(user, contact) do
    __MODULE__
    |> where([i], i.user_id == ^user.id and i.contact_id == ^contact.id)
    |> Repo.one()
  end

  def add(user, contact) do
    %__MODULE__{}
    |> changeset(%{
      user_id: user.id,
      contact_id: contact.id
    })
    |> Repo.insert(
      on_conflict: :nothing,
      conflict_target: [:user_id, :contact_id]
    )
  end

  @spec set_name(User.t(), User.t(), binary) :: {:ok, t()} | Roster.error()
  def set_name(user, contact, name) do
    __MODULE__
    |> where([i], i.user_id == ^user.id and i.contact_id == ^contact.id)
    |> select([i], i)
    |> Repo.update_all([{:set, [name: name]}])
    |> case do
      {1, [i]} -> {:ok, i}
      {0, _} -> {:error, :not_found}
    end
  end

  def delete_pair(a, b) do
    __MODULE__
    |> with_pair(a, b)
    |> Repo.delete_all()
  end

  def friends_query(user) do
    User
    |> join(:left, [u], i in __MODULE__, on: u.id == i.contact_id)
    |> where([..., i], i.user_id == ^user.id)
  end

  def items_query(user) do
    Item
    |> where([i], i.user_id == ^user.id)
  end

  defp changeset(struct, params) do
    struct
    |> cast(params, @change_fields)
    |> foreign_key_constraint(:user_id)
    |> foreign_key_constraint(:contact_id)
  end

  defp with_pair(query, a, b) do
    from r in query,
      where:
        (r.user_id == ^a.id and r.contact_id == ^b.id) or
          (r.user_id == ^b.id and r.contact_id == ^a.id)
  end
end
