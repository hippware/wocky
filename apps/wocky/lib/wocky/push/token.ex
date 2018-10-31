defmodule Wocky.Push.Token do
  @moduledoc "Schema for push notification tokens."

  use Wocky.Repo.Schema

  alias Wocky.User

  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id
  schema "push_tokens" do
    field :device, :string, null: false
    field :token, :string, null: false
    field :valid, :boolean, null: false, default: true
    field :enabled_at, :utc_datetime
    field :disabled_at, :utc_datetime
    field :invalidated_at, :utc_datetime

    timestamps(updated_at: false)

    belongs_to :user, User
  end

  @type token :: binary

  @type t :: %Token{
          user_id: Wocky.User.id(),
          device: Wocky.User.device(),
          token: token,
          valid: boolean,
          enabled_at: DateTime,
          disabled_at: DateTime,
          invalidated_at: DateTime
        }

  @register_attrs [:user_id, :device, :token]

  @doc false
  def register_changeset(attrs) do
    %Token{}
    |> cast(attrs, @register_attrs)
    |> validate_required(@register_attrs)
    |> foreign_key_constraint(:user_id)
    |> put_change(:enabled_at, DateTime.utc_now())
    |> put_change(:valid, true)
  end
end
