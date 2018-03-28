defmodule Wocky.Bot.Share do
  @moduledoc "Represents an bot being shared to a user"

  use Wocky.Repo.Schema

  import Ecto.Query

  alias Ecto.Changeset
  alias Wocky.Bot
  alias Wocky.Repo
  alias Wocky.User
  alias __MODULE__

  @foreign_key_type :binary_id
  @primary_key false
  schema "bot_shares" do
    field :user_id, :binary_id, primary_key: true
    field :bot_id, :binary_id, primary_key: true
    field :geofence, :boolean

    timestamps()

    belongs_to :sharer, User
    belongs_to :user, User, define_field: false
    belongs_to :bot, Bot, define_field: false
  end

  @type t :: %Share{}

  @spec changeset(t, map) :: Changeset.t()
  def changeset(struct, params) do
    struct
    |> cast(params, [:user_id, :bot_id, :sharer_id, :geofence])
    |> validate_required([:user_id, :bot_id, :sharer_id, :geofence])
    |> foreign_key_constraint(:user_id)
    |> foreign_key_constraint(:bot_id)
    |> foreign_key_constraint(:sharer_id)
  end

  @spec exists?(User.t(), Bot.t()) :: boolean
  def exists?(user, bot) do
    get(user, bot) != nil
  end

  @spec get(User.t(), Bot.t()) :: t | nil
  def get(user, bot) do
    Repo.get_by(Share, user_id: user.id, bot_id: bot.id)
  end

  @spec put(User.t(), Bot.t(), User.t(), boolean) :: :ok | no_return
  def put(user, bot, from, geofence \\ false) do
    %Share{}
    |> changeset(%{
      bot_id: bot.id,
      user_id: user.id,
      sharer_id: from.id,
      geofence: geofence
    })
    |> Repo.insert!(on_conflict: :nothing, conflict_target: [:user_id, :bot_id])

    :ok
  end

  @spec delete(User.t(), Bot.t()) :: :ok
  def delete(user, bot) do
    Share
    |> where(user_id: ^user.id, bot_id: ^bot.id)
    |> Repo.delete_all()

    :ok
  end
end
