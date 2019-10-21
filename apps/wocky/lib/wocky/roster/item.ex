defmodule Wocky.Roster.Item do
  @moduledoc """
  DB interface module for roster items
  """

  use Wocky.Repo.Schema

  import EctoEnum

  alias Ecto.Changeset
  alias Wocky.Account.User

  @share_types [:disabled, :always, :nearby]

  defenum(LocationShareTypeEnum, :location_share_type, @share_types)

  @foreign_key_type :binary_id
  schema "roster_items" do
    field :name, :binary, default: ""
    # TODO The share_id field is used to support the legacy sharing API. It
    # should be removed when that API is decommissioned.
    field :share_id, :integer
    field :share_type, LocationShareTypeEnum, null: false, default: :disabled
    field :share_migrated, :boolean, null: false, default: true
    field :share_changed_at, :utc_datetime_usec

    belongs_to :user, User
    belongs_to :contact, User

    timestamps()
  end

  @type name :: binary()
  @type share_type :: :disabled | :always | :nearby

  @type t :: %__MODULE__{
          user_id: User.id(),
          contact_id: User.id(),
          name: name(),
          share_type: share_type(),
          share_migrated: boolean(),
          updated_at: DateTime.t()
        }

  @update_fields [:name, :share_id, :share_type]
  @insert_fields [:user_id, :contact_id | @update_fields]

  @spec insert_changeset(map()) :: Changeset.t()
  def insert_changeset(params),
    do: changeset(%__MODULE__{}, @insert_fields, params)

  @spec update_changeset(Item.t(), map()) :: Changeset.t()
  def update_changeset(struct, params),
    do: changeset(struct, @update_fields, params)

  defp changeset(struct, fields, params) do
    struct
    |> cast(params, fields)
    |> validate_inclusion(:share_type, @share_types)
    |> foreign_key_constraint(:user_id)
    |> foreign_key_constraint(:contact_id)
    |> maybe_set_share_metadata()
  end

  defp maybe_set_share_metadata(changeset) do
    if get_change(changeset, :share_type) do
      changeset
      |> put_change(:share_migrated, true)
      |> put_change(:share_changed_at, DateTime.utc_now())
    else
      changeset
    end
  end
end
