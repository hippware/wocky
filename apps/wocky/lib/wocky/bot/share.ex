defmodule Wocky.Bot.Share do
  @moduledoc "Represents an bot being shared to a user"

  use Wocky.Repo.Model

  alias Wocky.Bot
  alias Wocky.User
  alias __MODULE__, as: Share

  @foreign_key_type :binary_id
  @primary_key false
  schema "bot_shares" do
    field :user_id, :binary_id, primary_key: true
    field :bot_id,  :binary_id, primary_key: true

    timestamps()

    belongs_to :sharer, User
    belongs_to :user, User, define_field: false
    belongs_to :bot, Bot, define_field: false
  end

  @type t :: %Share{}

  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [:user_id, :bot_id, :sharer_id])
    |> validate_required([:user_id, :bot_id, :sharer_id])
  end

  def put(bot, to, from) do
    %Share{}
    |> changeset(%{bot_id: bot.id, user_id: to.id, sharer_id: from.id})
    |> Repo.insert!(on_conflict: :nothing, conflict_target: [:user_id, :bot_id])

    :ok
  end

  def exists?(user, bot) do
    Repo.get_by(Share, user_id: user.id, bot_id: bot.id) != nil
  end
end
