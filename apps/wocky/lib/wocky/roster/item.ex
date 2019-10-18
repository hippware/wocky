defmodule Wocky.Roster.Item do
  @moduledoc """
  DB interface module for roster items
  """

  use Wocky.Repo.Schema

  import EctoEnum

  alias Ecto.Changeset
  alias Wocky.Account.User

  defenum(LocationShareTypeEnum, :location_share_type, [
    :disabled,
    :always,
    :nearby
  ])

  @foreign_key_type :binary_id
  schema "roster_items" do
    field :name, :binary, default: ""
    field :share_type, LocationShareTypeEnum, null: false, default: :disabled
    field :share_migrated, :boolean, null: false, default: true

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

  @insert_fields [:user_id, :contact_id, :name]

  @spec insert_changeset(map()) :: Changeset.t()
  def insert_changeset(params) do
    %__MODULE__{}
    |> cast(params, @insert_fields)
    |> foreign_key_constraint(:user_id)
    |> foreign_key_constraint(:contact_id)
  end
end
