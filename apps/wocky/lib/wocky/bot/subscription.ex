defmodule Wocky.Bot.Subscription do
  @moduledoc "Represents a subscription relationship between a User and a Bot"

  use Elixometer
  use Wocky.Repo.Schema

  import Ecto.Query

  alias Ecto.Changeset
  alias Wocky.Bot
  alias Wocky.Repo
  alias Wocky.User

  @foreign_key_type :binary_id
  @primary_key false
  schema "bot_subscriptions" do
    field :user_id, :binary_id, primary_key: true
    field :bot_id, :binary_id, primary_key: true
    field :visitor, :boolean, default: false
    field :visited_at, :utc_datetime
    field :departed_at, :utc_datetime

    timestamps()

    belongs_to :user, User, define_field: false
    belongs_to :bot, Bot, define_field: false
  end

  @type t :: %Subscription{}
  @type state :: nil | :subscribed | :visiting

  @spec changeset(t, map) :: Changeset.t()
  def changeset(struct, params) do
    struct
    |> cast(params, [:user_id, :bot_id])
    |> validate_required([:user_id, :bot_id])
    |> foreign_key_constraint(:user_id)
    |> foreign_key_constraint(:bot_id)
  end

  @spec state(User.t(), Bot.t()) :: state()
  def state(user, bot) do
    case get(user, bot) do
      nil -> nil
      %Subscription{visitor: true} -> :visiting
      %Subscription{} -> :subscribed
    end
  end

  @spec get(User.t(), Bot.t()) :: t | nil
  def get(user, bot) do
    Repo.get_by(Subscription, user_id: user.id, bot_id: bot.id)
  end

  @spec visit(User.t(), Bot.t()) :: :ok
  def visit(user, bot) do
    update_counter("bot.geofence.visit", 1)
    visit(user, bot, true)
  end

  @spec depart(User.t(), Bot.t()) :: :ok
  def depart(user, bot) do
    update_counter("bot.geofence.depart", 1)
    visit(user, bot, false)
  end

  @spec depart_all(User.t()) :: :ok
  def depart_all(user) do
    now = DateTime.utc_now()

    Subscription
    |> where(user_id: ^user.id, visitor: true)
    |> Repo.update_all(set: [visitor: false, departed_at: now, updated_at: now])
  end

  defp visit(user, bot, enter) do
    now = DateTime.utc_now()

    timestamps =
      if enter do
        [visited_at: now, departed_at: nil]
      else
        [departed_at: now]
      end

    Subscription
    |> where(user_id: ^user.id, bot_id: ^bot.id)
    |> Repo.update_all(set: [visitor: enter, updated_at: now] ++ timestamps)

    :ok
  end

  @spec put(User.t(), Bot.t()) :: :ok | no_return
  def put(user, bot) do
    %{user_id: user.id, bot_id: bot.id}
    |> make_changeset()
    |> Repo.insert!(
      on_conflict: [
        set: [updated_at: DateTime.utc_now()]
      ],
      conflict_target: [:user_id, :bot_id]
    )

    update_counter("bot.subscription.subscribe", 1)

    :ok
  end

  defp make_changeset(changes), do: changeset(%Subscription{}, changes)

  @spec delete(User.t(), Bot.t()) :: :ok | {:error, any}
  def delete(%User{id: id}, %Bot{user_id: id}), do: {:error, :denied}

  def delete(user, bot) do
    Subscription
    |> where(user_id: ^user.id, bot_id: ^bot.id)
    |> Repo.delete_all()

    update_counter("bot.subscription.unsubscribe", 1)

    :ok
  end
end
