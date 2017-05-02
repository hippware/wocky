defmodule Wocky.Bot.TempSubscription do
  @moduledoc "Represents a temporary subscription between a User and a Bot"

  use Wocky.Repo.Model

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

  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [:bot_id, :user_id, :resource, :node])
    |> validate_required([:bot_id, :user_id, :resource, :node])
  end

  def exists?(user, bot) do
    Repo.get_by(
      TempSubscription,
      user_id: user.id, resource: user.resource, bot_id: bot.id
    ) != nil
  end

  def put(user, bot, node) do
    Logger.error("""
    Creating temp subscription for user #{user.handle} to bot #{bot.shortname}
    """)
    data = %{
      user_id: user.id,
      resource: user.resource,
      bot_id: bot.id,
      node: node
    }

    %TempSubscription{}
    |> changeset(data)
    |> Repo.insert!(on_conflict: :replace_all,
                    conflict_target: [:bot_id, :user_id])

    :ok
  end

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

  def delete(user, bot) do
    TempSubscription
    |> where(user_id: ^user.id, resource: ^user.resource, bot_id: ^bot.id)
    |> Repo.delete_all

    :ok
  end
end
