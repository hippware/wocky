defmodule Wocky.Push.Token do
  @moduledoc "Schema for push notification tokens."

  use Wocky.Repo.Schema

  alias Wocky.User


  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id
  schema "push_tokens" do
    field :resource,  :string, null: false
    field :platform,  :string, null: false
    field :token,     :string, null: false
    field :valid,     :boolean, null: false, default: true
    field :enabled_at,     :utc_datetime
    field :disabled_at,    :utc_datetime
    field :invalidated_at, :utc_datetime

    timestamps(updated_at: false)

    belongs_to :user, User
  end

  @type token :: binary
  @type platform :: binary # "apple" | "google"

  @type t :: %Token{
    user_id:        Wocky.User.id,
    resource:       Wocky.User.resource,
    platform:       platform,
    token:          token,
    valid:          boolean,
    enabled_at:     DateTime,
    disabled_at:    DateTime,
    invalidated_at: DateTime
  }

  @register_attrs [:user_id, :resource, :platform, :token]

  @doc false
  def register_changeset(attrs) do
    %Token{}
    |> cast(attrs, @register_attrs)
    |> validate_required(@register_attrs)
    |> validate_inclusion(:platform, ["apple"])
    |> foreign_key_constraint(:user_id)
    |> put_change(:enabled_at, DateTime.utc_now)
    |> put_change(:valid, true)
  end
end
