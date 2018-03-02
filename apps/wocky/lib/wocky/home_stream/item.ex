defmodule Wocky.HomeStream.Item do
  @moduledoc """
  DB interface module for home stream items
  """

  use Wocky.Repo.Schema

  import EctoHomoiconicEnum, only: [defenum: 2]

  defenum ClassEnum, [:item, :deleted, :ref_update]

  @foreign_key_type :binary_id
  schema "home_stream_items" do
    field :key, :string
    field :from_jid, :binary, default: ""
    field :stanza, :binary, default: ""
    field :class, ClassEnum, default: :item

    belongs_to :user, Wocky.User
    belongs_to :reference_user, Wocky.User, foreign_key: :reference_user_id
    belongs_to :reference_bot, Wocky.Bot, foreign_key: :reference_bot_id
    # This field points to half of a composite foreign key for Wocky.Bot.Item
    # which Ecto doesn't natively support at the moment:
    field :reference_bot_item_id, :string

    timestamps()
  end

  @type key :: binary
  @type class :: :item | :deleted | :ref_update

  @type t :: %Item{
          user_id: Wocky.User.id(),
          key: key,
          from_jid: binary,
          stanza: binary,
          class: class,
          updated_at: DateTime.t(),
          reference_user: Wocky.User.t(),
          reference_bot: Wocky.Bot.t(),
          reference_bot_item_id: Wocky.Repo.ID.t()
        }

  @change_fields [
    :user_id,
    :key,
    :from_jid,
    :stanza,
    :class,
    :reference_user_id,
    :reference_bot_id,
    :reference_bot_item_id,
    :created_at,
    :updated_at
  ]

  @delete_changes [
    class: :deleted,
    stanza: "",
    from_jid: "",
    reference_user_id: nil,
    reference_bot_id: nil,
    reference_bot_item_id: nil
  ]

  def writable_fields, do: @change_fields

  def changeset(struct, params) do
    struct
    |> cast(params, @change_fields)
    |> foreign_key_constraint(:reference_user_id)
    |> foreign_key_constraint(:reference_bot_id)
  end

  # `update_all` does not set the `updated_at` field so we need to do it
  # ourselves
  def delete_changes do
    Keyword.put(@delete_changes, :updated_at, DateTime.utc_now())
  end

  def delete_changeset(struct) do
    changeset(struct, Map.new(delete_changes()))
  end
end
