defmodule Wocky.Roster.Invitation do
  @moduledoc """
  DB interface module for roster invitations
  """

  use Wocky.Repo.Schema

  import Ecto.Query

  alias Wocky.{Repo, User}

  @foreign_key_type :binary_id
  schema "user_invitations" do
    belongs_to :user, User
    belongs_to :invitee, User

    timestamps()
  end

  @type name :: binary

  @type t :: %__MODULE__{
          user: User.t(),
          invitee: User.t(),
          created_at: DateTime.t(),
          updated_at: DateTime.t()
        }

  @change_fields [:user_id, :invitee_id]

  def add(user, target) do
    %__MODULE__{}
    |> changeset(%{
      user_id: user.id,
      invitee_id: target.id
    })
    |> Repo.insert(
      on_conflict: :nothing,
      conflict_target: [:user_id, :invitee_id]
    )

    :ok
  end

  @spec delete_pair(User.t(), User.t()) :: :ok
  def delete_pair(a, b) do
    __MODULE__
    |> with_pair(a, b)
    |> Repo.delete_all()
  end

  def sent_query(user) do
    __MODULE__
    |> where([r], r.user_id == ^user.id)
  end

  def received_query(user) do
    __MODULE__
    |> where([r], r.invitee_id == ^user.id)
  end

  defp changeset(struct, params) do
    struct
    |> cast(params, @change_fields)
    |> foreign_key_constraint(:user_id)
    |> foreign_key_constraint(:invitee_id)
  end

  defp with_pair(query, a, b) do
    from r in query,
      where:
        (r.user_id == ^a.id and r.invitee_id == ^b.id) or
          (r.user_id == ^b.id and r.invitee_id == ^a.id)
  end
end
