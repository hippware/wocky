defmodule Wocky.Contacts.Relationship do
  @moduledoc """
  DB interface module for roster items
  """

  use Wocky.Repo.Schema

  import EctoEnum

  alias Wocky.Account.User
  alias Wocky.Repo.Timestamp

  @share_types [:disabled, :always, :nearby]

  defenum(LocationShareTypeEnum, :location_share_type, @share_types)

  @states [:invited, :friend, :blocked]

  defenum(ContactStateEnum, :state, @states)

  @foreign_key_type :binary_id
  schema "roster_items" do
    field :state, ContactStateEnum, null: false
    field :share_type, LocationShareTypeEnum, null: false, default: :disabled
    field :share_changed_at, :utc_datetime_usec
    field :nearby_distance, :integer, default: 2000
    field :nearby_cooldown, :integer, default: :timer.hours(2)
    field :nearby_last_start_notification, :utc_datetime_usec
    field :nearby, :boolean

    # TODO These fields are used to support the legacy sharing API. They
    # should be removed when that API is decommissioned.
    field :share_id, :integer
    field :share_migrated, :boolean, null: false, default: true

    belongs_to :user, User
    belongs_to :contact, User

    timestamps()
  end

  @type state :: ContactStateEnum.t()
  @type share_type :: LocationShareTypeEnum.t()

  @type t :: %__MODULE__{
          user_id: User.id(),
          contact_id: User.id(),
          state: state(),
          share_type: share_type(),
          share_migrated: boolean(),
          updated_at: DateTime.t(),
          nearby_distance: integer(),
          nearby_cooldown: integer(),
          nearby_last_start_notification: DateTime.t(),
          nearby: boolean()
        }

  @fields [
    :user_id,
    :contact_id,
    :state,
    :share_type,
    :nearby_distance,
    :nearby_cooldown,
    :nearby_last_start_notification,
    :nearby
  ]

  def share_types, do: @share_types

  defp build_assoc(user, contact) do
    Ecto.build_assoc(user, :relationships, %{contact_id: User.id(contact)})
  end

  @spec error_changeset(User.t(), User.tid(), map(), String.t()) ::
          Changeset.t()
  def error_changeset(user, contact, params, message) do
    user
    |> build_assoc(contact)
    |> cast(params, @fields)
    |> add_error(:contact_id, message)
  end

  @spec upsert_changeset(User.t(), User.tid(), state(), share_type()) ::
          Changeset.t()
  def upsert_changeset(user, contact, state, share_type) do
    user
    |> build_assoc(contact)
    |> changeset(%{state: state, share_type: share_type})
  end

  @spec changeset(t(), map()) :: Changeset.t()
  def changeset(struct, params) do
    struct
    |> cast(params, @fields)
    |> validate_inclusion(:state, @states)
    |> validate_inclusion(:share_type, @share_types)
    |> assoc_constraint(:user)
    |> assoc_constraint(:contact)
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
