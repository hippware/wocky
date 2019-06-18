defmodule Wocky.Relations.Subscription do
  @moduledoc "Represents a subscription relationship between a User and a Bot"

  use Wocky.Repo.Schema

  alias Ecto.Changeset
  alias Wocky.Account.User
  alias Wocky.Bots.Bot

  @foreign_key_type :binary_id
  @primary_key false
  schema "bot_subscriptions" do
    field :user_id, :binary_id, primary_key: true
    field :bot_id, :binary_id, primary_key: true
    field :visitor, :boolean, default: false
    field :visited_at, :utc_datetime_usec
    field :departed_at, :utc_datetime_usec

    timestamps()

    belongs_to :user, User, define_field: false
    belongs_to :bot, Bot, define_field: false
  end

  @type t :: %__MODULE__{}
  @type state :: nil | :subscribed | :visiting

  @spec changeset(t, map) :: Changeset.t()
  def changeset(struct, params) do
    struct
    |> cast(params, [:user_id, :bot_id])
    |> validate_required([:user_id, :bot_id])
    |> foreign_key_constraint(:user_id)
    |> foreign_key_constraint(:bot_id)
  end
end
