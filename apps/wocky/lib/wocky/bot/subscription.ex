defmodule Wocky.Bot.Subscription do
  @moduledoc "Represents a subscription relationship between a User and a Bot"

  use Wocky.Repo.Model

  alias Ecto.Changeset
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

  @spec changeset(t, map) :: Changeset.t
  def changeset(struct, params) do
    struct
    |> cast(params, [:user_id, :bot_id])
    |> validate_required([:user_id, :bot_id])
    |> foreign_key_constraint(:user_id)
    |> foreign_key_constraint(:bot_id)
  end

  @spec exists?(User.t, Bot.t) :: boolean
  def exists?(user, bot) do
    get(user, bot) != nil
  end

  @spec get(User.t, Bot.t) :: t | nil
  def get(user, bot) do
    Repo.get_by(Subscription, user_id: user.id, bot_id: bot.id)
  end

  @spec put(User.t, Bot.t) :: :ok | no_return
  def put(user, bot) do
    %Subscription{}
    |> changeset(%{user_id: user.id, bot_id: bot.id})
    |> Repo.insert!(on_conflict: :nothing, conflict_target: [:user_id, :bot_id])

    :ok
  end

  @spec delete(User.t, Bot.t) :: :ok
  def delete(user, bot) do
    Subscription
    |> where(user_id: ^user.id, bot_id: ^bot.id)
    |> Repo.delete_all

    :ok
  end
end
