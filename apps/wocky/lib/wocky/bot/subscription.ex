defmodule Wocky.Bot.Subscription do
  @moduledoc "Represents a subscription relationship between a User and a Bot"

  use Wocky.Repo.Model

  alias Wocky.Bot
  alias Wocky.User
  alias __MODULE__, as: Subscription

  @foreign_key_type :binary_id
  @primary_key false
  schema "bot_subscriptions" do
    field :user_id, :binary_id, primary_key: true
    field :bot_id,  :binary_id, primary_key: true

    timestamps()

    belongs_to :user, User, define_field: false
    belongs_to :bot, Bot, define_field: false
  end

  @type t :: %Subscription{}

  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [:user_id, :bot_id])
    |> validate_required([:user_id, :bot_id])
  end

  def exists?(user, bot) do
    Repo.get_by(Subscription, user_id: user.id, bot_id: bot.id) != nil
  end

  def put(user, bot) do
    %Subscription{}
    |> changeset(%{user_id: user.id, bot_id: bot.id})
    |> Repo.insert!(on_conflict: :nothing, conflict_target: [:user_id, :bot_id])

    :ok
  end

  def delete(user, bot) do
    Subscription
    |> where(user_id: ^user.id, bot_id: ^bot.id)
    |> Repo.delete_all

    :ok
  end
end
