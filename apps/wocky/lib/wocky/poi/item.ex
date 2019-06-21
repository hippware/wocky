defmodule Wocky.POI.Item do
  @moduledoc "Represents an item published to a bot"

  use Wocky.Repo.Schema

  alias Ecto.Changeset

  @foreign_key_type :binary_id
  @primary_key {:id, :binary_id, autogenerate: false}
  schema "bot_items" do
    field :content, :string
    field :image_url, :string

    timestamps()

    belongs_to :bot, Wocky.POI.Bot
    belongs_to :user, Wocky.Account.User
  end

  @type id :: binary
  @type t :: %__MODULE__{}

  @spec changeset(t(), map()) :: Changeset.t()
  def changeset(struct, params) do
    struct
    |> cast(params, [:id, :bot_id, :user_id, :content, :image_url])
    |> validate_required([:id, :bot_id, :user_id])
    |> validate_required_inclusion([:content, :image_url])
    |> foreign_key_constraint(:bot_id)
    |> foreign_key_constraint(:user_id)
  end
end
