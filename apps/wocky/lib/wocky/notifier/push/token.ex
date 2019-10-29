defmodule Wocky.Notifier.Push.Token do
  @moduledoc "Schema for push notification tokens."

  use Wocky.Repo.Schema

  import Ecto.Query
  import EctoEnum

  alias Wocky.Account.User
  alias Wocky.Repo

  defenum(PushServicePlatformEnum, :push_service_platform, [
    :apns,
    :fcm
  ])

  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id
  schema "push_tokens" do
    field :device, :string, null: false
    field :token, :string, null: false
    field :platform, PushServicePlatformEnum, null: false, default: :apns
    field :dev_mode, :boolean, null: false, default: false
    field :valid, :boolean, null: false, default: true
    field :enabled_at, :utc_datetime_usec
    field :disabled_at, :utc_datetime_usec
    field :invalidated_at, :utc_datetime_usec

    timestamps(updated_at: false)

    belongs_to :user, User
  end

  @type token :: binary()
  @type platform :: :apns | :platform

  @type t :: %__MODULE__{
          user_id: User.id(),
          device: User.device(),
          token: token(),
          platform: platform(),
          dev_mode: boolean(),
          valid: boolean(),
          enabled_at: DateTime.t(),
          disabled_at: DateTime.t(),
          invalidated_at: DateTime.t()
        }

  @required_attrs [:user_id, :device, :token]

  @register_attrs @required_attrs ++ [:platform, :dev_mode]

  @doc false
  def register_changeset(attrs) do
    attrs =
      attrs
      |> Enum.reject(fn {_, v} -> is_nil(v) end)
      |> Enum.into(%{})

    %__MODULE__{}
    |> cast(attrs, @register_attrs)
    |> validate_required(@required_attrs)
    |> foreign_key_constraint(:user_id)
    |> put_change(:enabled_at, DateTime.utc_now())
    |> put_change(:valid, true)
  end

  @spec all_for_user(User.t()) :: [Token]
  def all_for_user(user) do
    __MODULE__
    |> where(user_id: ^user.id)
    |> where(valid: true)
    |> Repo.all()
  end
end
