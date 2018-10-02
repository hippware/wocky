defmodule Wocky.User.InviteCode do
  @moduledoc false

  use Wocky.Repo.Schema

  alias Ecto.Changeset
  alias Wocky.User
  alias __MODULE__

  @foreign_key_type :binary_id
  schema "user_invite_codes" do
    field :code, :string

    timestamps(updated_at: false)

    belongs_to :user, User
  end

  @type t :: %InviteCode{}

  @code_bytes 16

  @spec changeset(t, map) :: Changeset.t()
  def changeset(struct, params) do
    struct
    |> cast(params, [:user_id, :code])
    |> validate_required([:user_id, :code])
    |> foreign_key_constraint(:user_id)
  end

  @spec generate :: binary
  def generate do
    @code_bytes
    |> :crypto.strong_rand_bytes()
    |> Base.encode64()
  end
end
