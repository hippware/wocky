defmodule Wocky.Push.Token do
  @moduledoc "Schema for push notification tokens."

  use Wocky.Repo.Schema

  import EctoHomoiconicEnum, only: [defenum: 2]

  alias Wocky.User

  defenum PushServicePlatform, [
    :apns
  ]

  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id
  schema "push_tokens" do
    field :device, :string, null: false
    field :token, :string, null: false
    field :platform, PushServicePlatform, null: false, default: :apns
    field :dev_mode, :boolean, null: false, default: false
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

  @required_attrs [:user_id, :device, :token]

  @register_attrs @required_attrs ++ [:platform, :dev_mode]

  @doc false
  def register_changeset(attrs) do
    attrs =
      attrs
      |> Enum.reject(fn {_, v} -> is_nil(v) end)
      |> Enum.into(%{})

    %Token{}
    |> cast(attrs, @register_attrs)
    |> validate_required(@required_attrs)
    |> foreign_key_constraint(:user_id)
    |> put_change(:enabled_at, DateTime.utc_now())
    |> put_change(:valid, true)
  end
end
