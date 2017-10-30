defmodule Wocky.Bot.TempSubscription do
  @moduledoc "Represents a temporary subscription between a User and a Bot"

  use Wocky.Repo.Model

  alias Ecto.Changeset
  alias Wocky.Bot
  alias Wocky.User
  alias __MODULE__, as: TempSubscription

  require Logger

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

  @spec changeset(t, map) :: Changeset.t
  def changeset(struct, params) do
    struct
    |> cast(params, [:bot_id, :user_id, :resource, :node])
    |> validate_required([:bot_id, :user_id, :resource, :node])
    |> foreign_key_constraint(:user_id)
    |> foreign_key_constraint(:bot_id)
  end

  @spec exists?(User.t, Bot.t) :: boolean
  def exists?(user, bot) do
    get(user, bot) != nil
  end

  @spec get(User.t, Bot.t) :: t | nil
  def get(user, bot) do
    Repo.get_by(
      TempSubscription,
      user_id: user.id, resource: user.resource, bot_id: bot.id
    )
  end

  @spec get_all(Bot.t | User.t) :: [t]
  def get_all(%Bot{} = bot) do
    TempSubscription
    |> where(bot_id: ^bot.id)
    |> preload(:user)
    |> Repo.all
  end
  def get_all(%User{} = user) do
    TempSubscription
    |> where(user_id: ^user.id)
    |> preload(:bot)
    |> Repo.all
  end

  @spec put(User.t, Bot.t, atom | binary) :: :ok | no_return
  def put(user, bot, node) do
    data = %{
      user_id: user.id,
      resource: user.resource,
      bot_id: bot.id,
      node: to_string(node)
    }

    %TempSubscription{}
    |> changeset(data)
    |> Repo.insert!(on_conflict: :replace_all,
                    conflict_target: [:bot_id, :user_id, :resource])

    :ok
  end

  @spec delete(User.t | atom | binary) :: :ok
  def delete(%User{id: id, resource: resource}) do
    TempSubscription
    |> where(user_id: ^id, resource: ^resource)
    |> Repo.delete_all

    :ok
  end
  def delete(node) do
    TempSubscription
    |> where(node: ^to_string(node))
    |> Repo.delete_all

    :ok
  end

  @spec delete(User.t, Bot.t) :: :ok
  def delete(user, bot) do
    TempSubscription
    |> where(user_id: ^user.id, resource: ^user.resource, bot_id: ^bot.id)
    |> Repo.delete_all

    :ok
  end
end
