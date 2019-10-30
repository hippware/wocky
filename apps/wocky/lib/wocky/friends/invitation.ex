defmodule Wocky.Friends.Invitation do
  @moduledoc """
  DB interface module for roster invitations
  """

  use Wocky.Repo.Schema

  import Ecto.Query

  alias Wocky.Account.User
  alias Wocky.Friends.Friend
  alias Wocky.Repo

  @foreign_key_type :binary_id
  schema "user_invitations" do
    field :share_type, Friend.LocationShareTypeEnum,
      null: false,
      default: :disabled

    belongs_to :user, User
    belongs_to :invitee, User

    timestamps()
  end

  @type name :: binary

  @type t :: %__MODULE__{
          user: User.t(),
          invitee: User.t(),
          share_type: Friend.share_type(),
          created_at: DateTime.t(),
          updated_at: DateTime.t()
        }

  @change_fields [:user_id, :invitee_id, :share_type]

  @spec get(User.tid(), User.tid()) :: t() | nil
  def get(user, contact) do
    user_id = User.id(user)
    contact_id = User.id(contact)

    __MODULE__
    |> where(user_id: ^user_id)
    |> where(invitee_id: ^contact_id)
    |> Repo.one()
  end

  @spec add(User.tid(), User.tid(), Friend.share_type()) :: Repo.result(t())
  def add(user, target, share_type) do
    %__MODULE__{}
    |> changeset(%{
      user_id: User.id(user),
      invitee_id: User.id(target),
      share_type: share_type
    })
    |> Repo.insert(
      on_conflict: {:replace, [:share_type]},
      conflict_target: [:user_id, :invitee_id]
    )
  end

  def make_error(user, target, share_type, field, message) do
    %__MODULE__{}
    |> cast(
      %{
        user_id: User.id(user),
        invitee_id: User.id(target),
        share_type: share_type
      },
      @change_fields
    )
    |> add_error(field, message)
  end

  defp changeset(struct, params) do
    struct
    |> cast(params, @change_fields)
    |> foreign_key_constraint(:user_id)
    |> foreign_key_constraint(:invitee_id)
  end

  @spec delete_pair(User.tid(), User.tid()) :: :ok
  def delete_pair(a, b) do
    __MODULE__
    |> with_pair(a, b)
    |> Repo.delete_all()
  end

  defp with_pair(query, a, b) do
    a_id = User.id(a)
    b_id = User.id(b)

    from r in query,
      where:
        (r.user_id == ^a_id and r.invitee_id == ^b_id) or
          (r.user_id == ^b_id and r.invitee_id == ^a_id)
  end
end
