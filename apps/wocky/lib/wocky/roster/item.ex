defmodule Wocky.Roster.Item do
  @moduledoc """
  DB interface module for roster items
  """

  use Wocky.Repo.Schema

  import Ecto.Changeset
  import Ecto.Query
  import EctoEnum

  alias Ecto.Changeset
  alias Wocky.Account.User
  alias Wocky.Repo
  alias Wocky.Repo.Timestamp

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

  @update_fields [:name, :share_type]
  @insert_fields [:user_id, :contact_id | @update_fields]

  @spec get(User.tid(), User.tid()) :: t() | nil
  def get(user, contact) do
    user_id = User.id(user)
    contact_id = User.id(contact)

    __MODULE__
    |> where(user_id: ^user_id)
    |> where(contact_id: ^contact_id)
    |> Repo.one()
  end

  @spec insert_changeset(map()) :: Changeset.t()
  def insert_changeset(params),
    do: changeset(%__MODULE__{}, @insert_fields, params)

  @spec update_changeset({User.id(), User.id()} | Item.t(), map()) ::
          Changeset.t()
  def update_changeset({uid, cid}, params) do
    case get(uid, cid) do
      nil -> error_changeset(uid, cid, params)
      item -> update_changeset(item, params)
    end
  end

  def update_changeset(%Item{} = struct, params),
    do: changeset(struct, @update_fields, params)

  defp error_changeset(uid, cid, params) do
    %__MODULE__{user_id: uid, contact_id: cid}
    |> cast(params, @update_fields)
    |> add_error(:contact_id, "must be a friend")
  end

  defp changeset(struct, fields, params) do
    struct
    |> cast(params, fields)
    |> validate_inclusion(:share_type, @share_types)
    |> foreign_key_constraint(:user_id)
    |> foreign_key_constraint(:contact_id)
    |> maybe_set_share_id(struct)
    |> maybe_set_share_metadata()
  end

  defp maybe_set_share_id(changeset, %{share_type: :disabled}) do
    if get_change(changeset, :share_type) do
      {_, user_id} = fetch_field(changeset, :user_id)
      {_, contact_id} = fetch_field(changeset, :contact_id)
      now_str = DateTime.utc_now() |> Timestamp.to_string!()
      id = :erlang.crc32([user_id, contact_id, now_str])
      put_change(changeset, :share_id, id)
    else
      changeset
    end
  end

  defp maybe_set_share_id(changeset, _item), do: changeset

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
