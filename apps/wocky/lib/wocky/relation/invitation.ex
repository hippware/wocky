defmodule Wocky.Relation.Invitation do
  @moduledoc "An invitation from a user to subscribe to a bot"

  use Wocky.Repo.Schema

  alias Ecto.Changeset
  alias Wocky.Account.User
  alias Wocky.POI.Bot

  @foreign_key_type :binary_id
  schema "bot_invitations" do
    field :accepted, :boolean

    belongs_to :user, User
    belongs_to :invitee, User
    belongs_to :bot, Bot

    timestamps()
  end

  @type id :: integer
  @type t :: %__MODULE__{}

  @spec changeset(t(), map()) :: Changeset.t()
  def changeset(struct, params) do
    struct
    |> cast(params, [:user_id, :bot_id, :invitee_id, :accepted])
    |> validate_required([:user_id, :bot_id, :invitee_id])
    |> foreign_key_constraint(:user_id)
    |> foreign_key_constraint(:bot_id)
    |> foreign_key_constraint(:invitee_id)
  end
end
