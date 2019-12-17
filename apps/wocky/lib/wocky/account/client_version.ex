defmodule Wocky.Account.ClientVersion do
  @moduledoc "Schema for tracking client versions"

  use Wocky.Repo.Schema

  @foreign_key_type :binary_id
  @primary_key false
  schema "client_versions" do
    field :user_id, :binary_id, null: false, primary_key: true
    field :device, :string, null: false, primary_key: true
    field :version, :string, null: false
    field :attributes, {:array, :string}, null: false, default: []

    timestamps()

    belongs_to :user, Wocky.Account.User, define_field: false
  end

  @type t :: %__MODULE__{}

  @fields [:user_id, :device, :version, :attributes]

  @spec changeset(t(), map()) :: Changeset.t()
  def changeset(struct, params) do
    struct
    |> cast(params, @fields)
    |> validate_required(@fields)
    |> validate_length(:version, min: 1)
    |> foreign_key_constraint(:user_id)
    |> unique_constraint(:device, name: :PRIMARY)
  end
end
