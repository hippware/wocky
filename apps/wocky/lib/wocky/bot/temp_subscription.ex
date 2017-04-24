defmodule Wocky.Bot.TempSubscription do
  @moduledoc "Represents a temporary subscription between a User and a Bot"

  use Wocky.Repo.Model

  alias Wocky.Bot
  alias Wocky.User
  alias __MODULE__, as: TempSubscription

  @foreign_key_type :binary_id
  @primary_key false
  schema "bot_temp_subscriptions" do
    field :bot_id,   :binary_id, primary_key: true
    field :user_id,  :binary_id, primary_key: true
    field :resource, :string, primary_key: true
    field :node,     :string

    timestamps()

    belongs_to :user, User, define_field: false
    belongs_to :bot, Bot, define_field: false
  end

  @type t :: %TempSubscription{}

  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [:bot_id, :user_id, :resource, :node])
    |> validate_required([:bot_id, :user_id, :resource, :node])
  end
end
