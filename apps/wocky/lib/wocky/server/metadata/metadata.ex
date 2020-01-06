defmodule Wocky.Server.Metadata do
  @moduledoc "Server metadata schema"

  use Wocky.Repo.Schema

  @primary_key {:key, :string, autogenerate: false}
  schema "metadata" do
    field :value, :string
    field :description, :string
  end

  @type key() :: String.t()
  @type value() :: String.t() | nil
  @type description() :: String.t() | nil

  @type t :: %__MODULE__{
          key: key(),
          value: value(),
          description: description()
        }

  @update_fields [:value, :description]

  @spec changeset(t(), map()) :: Changeset.t()
  def changeset(metadata, params) do
    metadata
    |> cast(params, @update_fields)
  end
end
