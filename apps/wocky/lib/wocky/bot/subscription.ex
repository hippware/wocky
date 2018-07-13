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
    field :guest, :boolean, default: false
    field :visitor, :boolean, default: false
    field :visited_at, :utc_datetime
    field :departed_at, :utc_datetime

    timestamps()

    belongs_to :user, User, define_field: false
    belongs_to :bot, Bot, define_field: false
  end

  @type t :: %Subscription{}
  @type state :: nil | :subscribed | :guest | :visitor

  @spec changeset(t, map) :: Changeset.t()
  def changeset(struct, params) do
    struct
    |> cast(params, [:user_id, :bot_id, :guest])
    |> validate_required([:user_id, :bot_id])
    |> foreign_key_constraint(:user_id)
    |> foreign_key_constraint(:bot_id)
  end

  @spec state(User.t(), Bot.t()) :: state()
  def state(user, bot) do
    case get(user, bot) do
      nil -> nil
      %Subscription{visitor: true} -> :visitor
      %Subscription{guest: true} -> :guest
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

  defp visit(user, bot, enter) do
    timestamps =
      if enter do
        [visited_at: DateTime.utc_now(), departed_at: nil]
      else
        [departed_at: DateTime.utc_now()]
      end

    Subscription
    |> where(user_id: ^user.id, bot_id: ^bot.id)
    |> Repo.update_all(
      set: [visitor: enter, updated_at: DateTime.utc_now()] ++ timestamps
    )

    :ok
  end

  @spec put(User.t(), Bot.t(), boolean()) :: :ok | no_return
  def put(user, bot, guest \\ false) do
    visitor_changes = maybe_set_visitor(guest)

    %{user_id: user.id, bot_id: bot.id, guest: guest}
    |> Map.merge(visitor_changes)
    |> make_changeset()
    |> Repo.insert!(
      on_conflict: [
        set:
          [guest: guest, updated_at: DateTime.utc_now()] ++
            Map.to_list(visitor_changes)
      ],
      conflict_target: [:user_id, :bot_id]
    )

    update_counter("bot.subscription.subscribe", 1)

    :ok
  end

  defp maybe_set_visitor(true), do: %{}

  defp maybe_set_visitor(false) do
    %{visitor: false, visited_at: nil, departed_at: nil}
  end

  defp make_changeset(changes), do: changeset(%Subscription{}, changes)

  @spec clear_guests(Bot.t()) :: :ok
  def clear_guests(bot) do
    Subscription
    |> where(bot_id: ^bot.id)
    |> Repo.update_all(
      set: [guest: false, visitor: false, updated_at: DateTime.utc_now()]
    )

    :ok
  end

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
